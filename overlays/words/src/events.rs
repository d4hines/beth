//! Shared message + state types used across the tap callback, the worker
//! thread, and the popover UI on the main thread.

use std::sync::{Arc, Mutex};

/// A single keyboard-derived event sent from the tap callback (main thread)
/// to the worker thread. No raw text is ever persisted; `Text` payloads live
/// only transiently in the worker's in-memory buffer.
#[derive(Debug)]
pub enum Ev {
    /// One or more printable characters were typed.
    Text(String),
    /// Backspace: remove the last character from the pending buffer.
    Backspace,
    /// Return/Enter: end the current line and flush.
    Newline,
    /// Navigation / focus change / shortcut: our linear model breaks, so
    /// flush what we have and start fresh.
    Boundary,
}

/// Per-app counts (before display-name/icon enrichment on the main thread).
#[derive(Clone, Default)]
pub struct AppRaw {
    pub bundle_id: String,
    pub tokens: i64,
    pub words: i64,
}

/// One bar in a range's histogram (an hour for Today, a day for Week/Month).
#[derive(Clone, Default)]
pub struct SeriesPoint {
    pub label: String,
    pub tokens: i64,
    pub words: i64,
}

/// Everything the dashboard needs for one time range (today / week / month).
#[derive(Clone, Default)]
pub struct RangeData {
    pub tokens: i64,
    pub words: i64,
    /// Totals for the equivalent previous period (for the delta).
    pub prev_tokens: i64,
    pub prev_words: i64,
    /// Per-day average across the range.
    pub avg_tokens: i64,
    pub avg_words: i64,
    pub series: Vec<SeriesPoint>,
    pub apps: Vec<AppRaw>,
}

/// The three ranges, recomputed by the worker after each flush.
#[derive(Clone, Default)]
pub struct Snapshot {
    pub today: RangeData,
    pub week: RangeData,
    pub month: RangeData,
}

/// Dashboard data, written by the worker and read by the main thread.
pub type SharedSnapshot = Arc<Mutex<Snapshot>>;

/// Bundle id of the frontmost app, refreshed (throttled) in the tap callback
/// and read by the worker at flush time to attribute counts.
pub type SharedFrontApp = Arc<Mutex<String>>;
