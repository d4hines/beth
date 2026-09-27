//! Word Counter — a macOS menu bar app that counts the words, characters, and
//! (locally tokenized) tokens you type, bucketed per hour and per app, into a
//! SQLite database. No typed text is ever written to disk.

mod db;
mod events;
mod tap;
mod ui;
mod worker;

use crossbeam_channel::unbounded;
use events::{SharedFrontApp, SharedSnapshot, Snapshot};
use objc2::rc::autoreleasepool;
use objc2_app_kit::{NSApplication, NSApplicationActivationPolicy, NSEventMask};
use objc2_foundation::{MainThreadMarker, NSDate, NSDefaultRunLoopMode};
use std::sync::{Arc, Mutex};
use std::thread;
use ui::Panel;

fn main() {
    let (tx, rx) = unbounded::<events::Ev>();
    let snapshot: SharedSnapshot = Arc::new(Mutex::new(Snapshot::default()));
    let front: SharedFrontApp = Arc::new(Mutex::new(String::new()));

    // Storage + tokenization run off the main thread.
    {
        let snapshot = snapshot.clone();
        let front = front.clone();
        thread::spawn(move || worker::run(rx, snapshot, front));
    }

    let mtm = MainThreadMarker::new().expect("main() runs on the main thread");
    let app = NSApplication::sharedApplication(mtm);
    // Accessory: live in the menu bar with no Dock icon.
    app.setActivationPolicy(NSApplicationActivationPolicy::Accessory);

    let panel = Panel::build(mtm, snapshot.clone());

    // Ask for Accessibility permission (shows the system dialog on first run),
    // then install the tap. If it isn't granted the tap can't be created; we
    // keep running so the popover still works and the user can grant + relaunch.
    let _ = tap::is_trusted(true);
    if let Err(e) = tap::install(tx.clone(), front.clone()) {
        eprintln!("word-counter: {e}");
    }

    // Finish launching so the status item appears, then pump events manually.
    // The manual loop keeps the tray, the tap's run-loop source, popover
    // interaction, and data refresh all on the main thread.
    #[allow(deprecated)]
    unsafe {
        app.finishLaunching();
    }

    loop {
        // Drain pending Cocoa events (this also services the tap's run-loop
        // source). Block up to 0.2s so we idle cheaply when nothing happens.
        // Each pass gets its own autorelease pool: unlike `app.run()`, a manual
        // loop has none, so autoreleased objects (the NSDate, events, anything
        // the tap callback creates) would otherwise accumulate forever.
        loop {
            let handled = autoreleasepool(|_| {
                let until = unsafe { NSDate::dateWithTimeIntervalSinceNow(0.2) };
                let event = unsafe {
                    app.nextEventMatchingMask_untilDate_inMode_dequeue(
                        NSEventMask::Any,
                        Some(&until),
                        NSDefaultRunLoopMode,
                        true,
                    )
                };
                match event {
                    Some(event) => {
                        unsafe { app.sendEvent(&event) };
                        true
                    }
                    None => false,
                }
            });
            if !handled {
                break;
            }
        }

        autoreleasepool(|_| panel.tick());
    }
}
