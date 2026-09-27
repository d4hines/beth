//! System-wide keyboard capture via a passive CGEventTap, plus frontmost-app
//! lookup and the Accessibility permission prompt.
//!
//! The tap is created in ListenOnly mode and the callback always returns
//! `None`, which the core-graphics trampoline turns into "pass the original
//! event through unchanged" — we never modify or swallow keystrokes.

use crate::events::{Ev, SharedFrontApp};
use core_foundation::base::TCFType;
use core_foundation::boolean::CFBoolean;
use core_foundation::dictionary::{CFDictionary, CFDictionaryRef};
use core_foundation::mach_port::CFMachPortRef;
use core_foundation::runloop::{kCFRunLoopCommonModes, CFRunLoop};
use core_foundation::string::{CFString, CFStringRef};
use core_graphics::event::{
    CGEventFlags, CGEventTap, CGEventTapLocation, CGEventTapOptions, CGEventTapPlacement,
    CGEventType, EventField,
};
use crossbeam_channel::Sender;
use foreign_types::ForeignType;
use objc2_app_kit::NSWorkspace;
use objc2_foundation::NSString;
use once_cell::sync::OnceCell;
use std::cell::Cell;
use std::ffi::c_void;
use std::os::raw::c_ulong;
use std::time::{Duration, Instant};

#[link(name = "CoreGraphics", kind = "framework")]
extern "C" {
    fn CGEventTapEnable(tap: CFMachPortRef, enable: bool);
    fn CGEventKeyboardGetUnicodeString(
        event: *const c_void,
        max_len: c_ulong,
        actual_len: *mut c_ulong,
        buf: *mut u16,
    );
}

#[link(name = "ApplicationServices", kind = "framework")]
extern "C" {
    fn AXIsProcessTrustedWithOptions(options: CFDictionaryRef) -> bool;
    static kAXTrustedCheckOptionPrompt: CFStringRef;
}

/// Wrapper so we can stash the tap's mach port in a global for re-enabling
/// from inside the callback if the system disables the tap.
struct PortHandle(CFMachPortRef);
unsafe impl Send for PortHandle {}
unsafe impl Sync for PortHandle {}
static TAP_PORT: OnceCell<PortHandle> = OnceCell::new();

// Virtual keycodes we treat specially.
const KC_RETURN: i64 = 36;
const KC_TAB: i64 = 48;
const KC_BACKSPACE: i64 = 51;
const KC_ESC: i64 = 53;
const KC_ENTER: i64 = 76; // keypad enter
const KC_FWD_DELETE: i64 = 117;

/// Returns true if the process is trusted for Accessibility. Passing `prompt`
/// shows the system dialog directing the user to grant it.
pub fn is_trusted(prompt: bool) -> bool {
    unsafe {
        let key = CFString::wrap_under_get_rule(kAXTrustedCheckOptionPrompt);
        let val = if prompt {
            CFBoolean::true_value()
        } else {
            CFBoolean::false_value()
        };
        let opts = CFDictionary::from_CFType_pairs(&[(key.as_CFType(), val.as_CFType())]);
        AXIsProcessTrustedWithOptions(opts.as_concrete_TypeRef())
    }
}

/// Create the event tap on the current thread's run loop and start delivering
/// events to `tx`. Must be called on the main thread (the tap callback also
/// queries NSWorkspace, which prefers the main thread).
pub fn install(tx: Sender<Ev>, front: SharedFrontApp) -> Result<(), String> {
    // Throttle frontmost-app lookups to a few per second regardless of typing
    // speed. Cell is fine: the callback only ever runs on the main thread.
    let last_front = Cell::new(Instant::now() - Duration::from_secs(1));

    let tap = CGEventTap::new(
        // Session-level tap: sees keystrokes as delivered to apps (both real
        // hardware input and anything injected into the session). More
        // reliable for text tracking than the lower-level HID tap.
        CGEventTapLocation::Session,
        CGEventTapPlacement::HeadInsertEventTap,
        CGEventTapOptions::ListenOnly,
        vec![CGEventType::KeyDown],
        move |_proxy, etype, event| {
            // The system can disable a tap; re-enable and move on.
            if matches!(
                etype,
                CGEventType::TapDisabledByTimeout | CGEventType::TapDisabledByUserInput
            ) {
                if let Some(p) = TAP_PORT.get() {
                    unsafe { CGEventTapEnable(p.0, true) };
                }
                return None;
            }

            if last_front.get().elapsed() >= Duration::from_millis(250) {
                last_front.set(Instant::now());
                if let Some(id) = frontmost_bundle_id() {
                    if let Ok(mut g) = front.lock() {
                        *g = id;
                    }
                }
            }

            let flags = event.get_flags();
            // Command/Control chords are shortcuts, not text. Treat as an edit
            // boundary and don't count the character.
            if flags.contains(CGEventFlags::CGEventFlagCommand)
                || flags.contains(CGEventFlags::CGEventFlagControl)
            {
                let _ = tx.send(Ev::Boundary);
                return None;
            }

            let keycode = event.get_integer_value_field(EventField::KEYBOARD_EVENT_KEYCODE);
            match keycode {
                KC_BACKSPACE => {
                    let _ = tx.send(Ev::Backspace);
                }
                KC_RETURN | KC_ENTER => {
                    let _ = tx.send(Ev::Newline);
                }
                // Arrows, home/end/page-up/down, tab, esc, forward-delete:
                // cursor moved or focus changed, so our linear buffer is stale.
                KC_TAB | KC_ESC | KC_FWD_DELETE | 115..=121 | 123..=126 => {
                    let _ = tx.send(Ev::Boundary);
                }
                _ => {
                    if let Some(s) = unicode_string(event) {
                        let _ = tx.send(Ev::Text(s));
                    }
                }
            }
            None
        },
    )
    .map_err(|_| {
        "Could not create the keyboard event tap. Grant Accessibility permission in \
         System Settings › Privacy & Security › Accessibility, then relaunch."
            .to_string()
    })?;

    let source = tap
        .mach_port
        .create_runloop_source(0)
        .map_err(|_| "Failed to create run loop source for the event tap.".to_string())?;

    let _ = TAP_PORT.set(PortHandle(tap.mach_port.as_concrete_TypeRef()));
    unsafe {
        CFRunLoop::get_current().add_source(&source, kCFRunLoopCommonModes);
    }
    tap.enable();
    // The tap must outlive this function; it lives for the whole process.
    std::mem::forget(tap);
    Ok(())
}

/// Bundle id of the frontmost application, e.g. "com.apple.Safari".
fn frontmost_bundle_id() -> Option<String> {
    unsafe {
        let ws = NSWorkspace::sharedWorkspace();
        let app = ws.frontmostApplication()?;
        let id = app.bundleIdentifier()?;
        Some(id.to_string())
    }
}

/// Human-friendly name for a bundle id, e.g. "com.google.Chrome" -> "Google
/// Chrome". Falls back to the last dotted component if the app can't be located
/// (e.g. an uninstalled app or the "unknown" sentinel). Must run on the main
/// thread (NSWorkspace).
pub fn app_display_name(bundle_id: &str) -> String {
    let fallback = || {
        bundle_id
            .rsplit('.')
            .next()
            .unwrap_or(bundle_id)
            .to_string()
    };
    unsafe {
        let ws = NSWorkspace::sharedWorkspace();
        let id = NSString::from_str(bundle_id);
        let Some(url) = ws.URLForApplicationWithBundleIdentifier(&id) else {
            return fallback();
        };
        match url.lastPathComponent() {
            Some(name) => {
                let s = name.to_string();
                s.strip_suffix(".app").map(str::to_string).unwrap_or(s)
            }
            None => fallback(),
        }
    }
}

/// The characters produced by a keyboard event, with control chars stripped.
fn unicode_string(event: &core_graphics::event::CGEvent) -> Option<String> {
    let mut buf = [0u16; 8];
    let mut actual: c_ulong = 0;
    unsafe {
        CGEventKeyboardGetUnicodeString(
            event.as_ptr() as *const c_void,
            buf.len() as c_ulong,
            &mut actual,
            buf.as_mut_ptr(),
        );
    }
    let n = (actual as usize).min(buf.len());
    if n == 0 {
        return None;
    }
    let s: String = String::from_utf16_lossy(&buf[..n])
        .chars()
        .filter(|c| !c.is_control())
        .collect();
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}
