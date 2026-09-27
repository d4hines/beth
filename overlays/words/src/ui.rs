//! The menu bar UI: an `NSStatusItem` whose button toggles an `NSPopover`
//! hosting a `WKWebView` (left-click) or shows a right-click menu with the
//! folder/quit actions. The web view renders `dashboard.html`; we push data
//! into it as JSON.

use crate::db;
use crate::events::{RangeData, SharedSnapshot, Snapshot};
use crate::tap;
use objc2::rc::Retained;
use objc2::runtime::{AnyObject, NSObject, NSObjectProtocol};
use objc2::{declare_class, msg_send_id, mutability, sel, ClassType, DeclaredClass};
use objc2_app_kit::{
    NSApplication, NSBitmapImageFileType, NSBitmapImageRep, NSCellImagePosition,
    NSCompositingOperation, NSControlStateValueOff, NSControlStateValueOn, NSEventMask,
    NSEventModifierFlags, NSEventType, NSImage, NSMenu, NSMenuItem, NSPopover, NSPopoverBehavior,
    NSStatusBar, NSStatusBarButton, NSStatusItem, NSVariableStatusItemLength, NSView,
    NSViewController, NSWorkspace,
};
use objc2_foundation::{
    MainThreadMarker, NSData, NSDataBase64EncodingOptions, NSDictionary, NSPoint, NSRect,
    NSRectEdge, NSSize, NSString, NSUserDefaults,
};
use objc2_service_management::{SMAppService, SMAppServiceStatus};
use objc2_web_kit::{WKWebView, WKWebViewConfiguration};
use std::cell::RefCell;
use std::collections::HashMap;

const HTML: &str = include_str!("dashboard.html");
/// Monochrome pencil for the menu bar (rendered as a template image).
const MENUBAR_PNG: &[u8] = include_bytes!("../assets/menubar-pencil@2x.png");
const WIDTH: f64 = 344.0;
const HEIGHT: f64 = 600.0;

/// Ivars for the Objective-C controller.
pub struct Ivars {
    popover: Retained<NSPopover>,
    button: Retained<NSStatusBarButton>,
    menu: Retained<NSMenu>,
    login_item: Retained<NSMenuItem>,
}

declare_class!(
    struct Controller;

    unsafe impl ClassType for Controller {
        type Super = NSObject;
        type Mutability = mutability::MainThreadOnly;
        const NAME: &'static str = "WCController";
    }

    impl DeclaredClass for Controller {
        type Ivars = Ivars;
    }

    unsafe impl Controller {
        // Left-click toggles the popover; right- or control-click shows the menu.
        #[method(togglePopover:)]
        fn toggle_popover(&self, _sender: Option<&AnyObject>) {
            let iv = self.ivars();
            let mtm = MainThreadMarker::new().unwrap();
            let event = NSApplication::sharedApplication(mtm).currentEvent();
            let is_secondary = event.as_ref().is_some_and(|e| {
                let ty = unsafe { e.r#type() };
                matches!(ty, NSEventType::RightMouseUp | NSEventType::RightMouseDown)
                    || unsafe { e.modifierFlags() }
                        .contains(NSEventModifierFlags::NSEventModifierFlagControl)
            });

            if is_secondary {
                if let Some(e) = event.as_deref() {
                    let view: &NSView = &iv.button;
                    unsafe { NSMenu::popUpContextMenu_withEvent_forView(&iv.menu, e, view) };
                }
                return;
            }

            if unsafe { iv.popover.isShown() } {
                unsafe { iv.popover.performClose(None) };
            } else {
                let view: &NSView = &iv.button;
                let bounds = iv.button.bounds();
                unsafe {
                    iv.popover
                        .showRelativeToRect_ofView_preferredEdge(bounds, view, NSRectEdge::MinY);
                }
            }
        }

        #[method(openFolder:)]
        fn open_folder(&self, _sender: Option<&AnyObject>) {
            let _ = std::process::Command::new("open").arg(db::db_dir()).spawn();
        }

        #[method(toggleLogin:)]
        fn toggle_login(&self, _sender: Option<&AnyObject>) {
            set_login_enabled(!login_enabled());
            mark_login_configured();
            let on = login_enabled();
            unsafe {
                self.ivars().login_item.setState(login_state(on));
            }
        }

        #[method(quitApp:)]
        fn quit_app(&self, _sender: Option<&AnyObject>) {
            std::process::exit(0);
        }
    }

    unsafe impl NSObjectProtocol for Controller {}
);

/// Owns all the retained AppKit/WebKit objects for the menu bar UI.
pub struct Panel {
    _status_item: Retained<NSStatusItem>,
    _controller: Retained<Controller>,
    popover: Retained<NSPopover>,
    webview: Retained<WKWebView>,
    button: Retained<NSStatusBarButton>,
    snapshot: SharedSnapshot,
    names: RefCell<HashMap<String, AppMeta>>,
}

impl Panel {
    pub fn build(mtm: MainThreadMarker, snapshot: SharedSnapshot) -> Panel {
        // Status item + its button.
        let status_bar = unsafe { NSStatusBar::systemStatusBar() };
        let status_item = unsafe { status_bar.statusItemWithLength(NSVariableStatusItemLength) };
        let button = unsafe { status_item.button(mtm) }
            .expect("status item should have a button");
        // Pencil icon as a template image (adapts to the menu bar's appearance),
        // with the word count as the button title beside it.
        let data = NSData::with_bytes(MENUBAR_PNG);
        if let Some(image) = NSImage::initWithData(NSImage::alloc(), &data) {
            unsafe {
                image.setTemplate(true);
                image.setSize(NSSize::new(18.0, 18.0));
                button.setImage(Some(&image));
                button.setImagePosition(NSCellImagePosition::NSImageLeft);
            }
        }
        // Also fire the action on right mouse up so we can show the context menu.
        if let Some(cell) = unsafe { button.cell() } {
            unsafe { cell.sendActionOn(NSEventMask::LeftMouseUp | NSEventMask::RightMouseUp) };
        }

        // Web view rendering the dashboard.
        let config = unsafe { WKWebViewConfiguration::new() };
        let frame = NSRect::new(NSPoint::new(0.0, 0.0), NSSize::new(WIDTH, HEIGHT));
        let webview =
            unsafe { WKWebView::initWithFrame_configuration(mtm.alloc(), frame, &config) };
        unsafe { webview.loadHTMLString_baseURL(&NSString::from_str(HTML), None) };

        // Popover hosting the web view via a plain view controller.
        let vc = unsafe { NSViewController::new(mtm) };
        let webview_as_view: &NSView = &webview;
        unsafe { vc.setView(webview_as_view) };
        let popover = unsafe { NSPopover::new(mtm) };
        unsafe {
            popover.setContentViewController(Some(&vc));
            popover.setContentSize(NSSize::new(WIDTH, HEIGHT));
            popover.setBehavior(NSPopoverBehavior::Transient);
            popover.setAnimates(true);
        }

        // Right-click menu.
        let menu = NSMenu::new(mtm);
        let item = |title: &str, action, key: &str| unsafe {
            NSMenuItem::initWithTitle_action_keyEquivalent(
                mtm.alloc(),
                &NSString::from_str(title),
                Some(action),
                &NSString::from_str(key),
            )
        };
        let login_item = item("Start at Login", sel!(toggleLogin:), "");
        let open_item = item("Open Database Folder", sel!(openFolder:), "");
        let quit_item = item("Quit Words", sel!(quitApp:), "q");
        menu.addItem(&login_item);
        menu.addItem(&NSMenuItem::separatorItem(mtm));
        menu.addItem(&open_item);
        menu.addItem(&NSMenuItem::separatorItem(mtm));
        menu.addItem(&quit_item);

        // On first launch, enable Start at Login by default.
        if !login_configured() {
            set_login_enabled(true);
            mark_login_configured();
        }
        unsafe { login_item.setState(login_state(login_enabled())) };

        // Controller wiring.
        let ivars = Ivars {
            popover: popover.clone(),
            button: button.clone(),
            menu: menu.clone(),
            login_item: login_item.clone(),
        };
        let this = mtm.alloc::<Controller>().set_ivars(ivars);
        let controller: Retained<Controller> = unsafe { msg_send_id![super(this), init] };

        let target: &AnyObject = &controller;
        unsafe {
            login_item.setTarget(Some(target));
            open_item.setTarget(Some(target));
            quit_item.setTarget(Some(target));
            button.setTarget(Some(target));
            button.setAction(Some(sel!(togglePopover:)));
        }

        Panel {
            _status_item: status_item,
            _controller: controller,
            popover,
            webview,
            button,
            snapshot,
            names: RefCell::new(HashMap::new()),
        }
    }

    /// Called from the main pump loop: refresh the title always, and push data
    /// into the web view while the popover is open.
    pub fn tick(&self) {
        let snap = self.snapshot.lock().unwrap().clone();
        unsafe {
            self.button
                .setTitle(&NSString::from_str(&format!(" {}", compact(snap.today.words))))
        };

        if unsafe { self.popover.isShown() } {
            let json = self.build_json(&snap);
            let js = format!("window.render({json})");
            unsafe {
                self.webview
                    .evaluateJavaScript_completionHandler(&NSString::from_str(&js), None)
            };
        }
    }

    fn build_json(&self, snap: &Snapshot) -> String {
        serde_json::json!({
            "today": self.range_json(&snap.today),
            "week": self.range_json(&snap.week),
            "month": self.range_json(&snap.month),
        })
        .to_string()
    }

    fn range_json(&self, r: &RangeData) -> serde_json::Value {
        let apps: Vec<serde_json::Value> = r
            .apps
            .iter()
            .map(|a| {
                let m = self.app_meta(&a.bundle_id);
                serde_json::json!({
                    "name": m.name,
                    "mono": mono(&m.name),
                    "tint": m.tint,
                    "icon": m.icon,
                    "tokens": a.tokens,
                    "words": a.words,
                })
            })
            .collect();
        let series: Vec<serde_json::Value> = r
            .series
            .iter()
            .map(|p| serde_json::json!({ "label": p.label, "tokens": p.tokens, "words": p.words }))
            .collect();
        serde_json::json!({
            "total": { "tokens": r.tokens, "words": r.words },
            "prev": { "tokens": r.prev_tokens, "words": r.prev_words },
            "avg": { "tokens": r.avg_tokens, "words": r.avg_words },
            "series": series,
            "apps": apps,
        })
    }

    /// Cached display name, tint, and icon (data URI) for a bundle id.
    fn app_meta(&self, bundle_id: &str) -> AppMeta {
        if let Some(hit) = self.names.borrow().get(bundle_id) {
            return hit.clone();
        }
        let meta = AppMeta {
            name: tap::app_display_name(bundle_id),
            tint: tint_for(bundle_id).to_string(),
            icon: app_icon_data_uri(bundle_id),
        };
        self.names
            .borrow_mut()
            .insert(bundle_id.to_string(), meta.clone());
        meta
    }
}

#[derive(Clone)]
struct AppMeta {
    name: String,
    tint: String,
    icon: Option<String>,
}

/// Compact count for the title, e.g. 3400 -> "3.4k".
fn compact(n: i64) -> String {
    if n < 1000 {
        n.to_string()
    } else if n < 1_000_000 {
        format!("{:.1}k", n as f64 / 1000.0).replace(".0k", "k")
    } else {
        format!("{:.1}M", n as f64 / 1_000_000.0)
    }
}

/// First alphanumeric character of the display name, uppercased.
fn mono(name: &str) -> String {
    name.chars()
        .find(|c| c.is_alphanumeric())
        .map(|c| c.to_uppercase().to_string())
        .unwrap_or_else(|| "?".to_string())
}

/// Deterministic badge tint for a bundle id (fallback when no icon is found).
fn tint_for(bundle_id: &str) -> &'static str {
    const TINTS: [&str; 8] = [
        "#5B7CFA", "#E8534E", "#7C5CD6", "#2F9E8F", "#E4B33E", "#3A9BDC", "#D0679E", "#5AA469",
    ];
    let h = bundle_id.bytes().fold(0u32, |a, b| a.wrapping_mul(31).wrapping_add(b as u32));
    TINTS[(h as usize) % TINTS.len()]
}

// ---- Launch at login (SMAppService) ----

const LOGIN_KEY: &str = "launchAtLoginConfigured";

fn login_service() -> Retained<SMAppService> {
    unsafe { SMAppService::mainAppService() }
}

/// Whether the login item is currently registered and enabled.
fn login_enabled() -> bool {
    unsafe { login_service().status() }.0 == SMAppServiceStatus::Enabled.0
}

fn set_login_enabled(enable: bool) {
    let svc = login_service();
    let result = if enable {
        unsafe { svc.registerAndReturnError() }
    } else {
        unsafe { svc.unregisterAndReturnError() }
    };
    if let Err(e) = result {
        let verb = if enable { "register" } else { "unregister" };
        eprintln!("word-counter: could not {verb} login item: {e:?}");
    }
}

fn login_state(on: bool) -> objc2_app_kit::NSControlStateValue {
    if on {
        NSControlStateValueOn
    } else {
        NSControlStateValueOff
    }
}

/// True once we've applied the default-on behavior at least once, so we don't
/// re-enable it after the user has turned it off.
fn login_configured() -> bool {
    let d = unsafe { NSUserDefaults::standardUserDefaults() };
    unsafe { d.objectForKey(&NSString::from_str(LOGIN_KEY)) }.is_some()
}

fn mark_login_configured() {
    let d = unsafe { NSUserDefaults::standardUserDefaults() };
    unsafe { d.setBool_forKey(true, &NSString::from_str(LOGIN_KEY)) };
}

/// The app's icon as a downscaled (44×44) PNG data URI, or None if the app
/// can't be located. Called once per bundle id (result is cached). Main thread.
#[allow(deprecated)] // lockFocus/unlockFocus: simplest reliable downscale path
fn app_icon_data_uri(bundle_id: &str) -> Option<String> {
    let side = 44.0;
    unsafe {
        let ws = NSWorkspace::sharedWorkspace();
        let url = ws.URLForApplicationWithBundleIdentifier(&NSString::from_str(bundle_id))?;
        let path = url.path()?;
        let icon: Retained<NSImage> = ws.iconForFile(&path);

        // Draw into a small image so the resulting PNG stays tiny.
        let target = NSImage::initWithSize(NSImage::alloc(), NSSize::new(side, side));
        target.lockFocus();
        let dst = NSRect::new(NSPoint::new(0.0, 0.0), NSSize::new(side, side));
        icon.drawInRect_fromRect_operation_fraction(
            dst,
            NSRect::new(NSPoint::new(0.0, 0.0), NSSize::new(0.0, 0.0)),
            NSCompositingOperation::SourceOver,
            1.0,
        );
        target.unlockFocus();

        let tiff = target.TIFFRepresentation()?;
        let rep = NSBitmapImageRep::initWithData(NSBitmapImageRep::alloc(), &tiff)?;
        let props = NSDictionary::new();
        let png = rep.representationUsingType_properties(NSBitmapImageFileType::PNG, &props)?;
        let b64 = png.base64EncodedStringWithOptions(NSDataBase64EncodingOptions(0));
        Some(format!("data:image/png;base64,{}", b64))
    }
}
