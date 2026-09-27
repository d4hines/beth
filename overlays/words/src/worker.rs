//! Background worker: owns the transient text buffer, the tokenizer, and the
//! database connection. Receives keystroke events, and on each boundary
//! tokenizes the buffered text, writes counts, and discards the text.

use crate::db;
use crate::events::{Ev, SharedFrontApp, SharedSnapshot};
use crossbeam_channel::{Receiver, RecvTimeoutError};
use rusqlite::Connection;
use std::time::Duration;
use tiktoken_rs::CoreBPE;

/// Soft threshold: once the buffer passes this, flush completed words.
const FLUSH_CHARS: usize = 240;
/// Hard cap: flush everything even mid-word, to bound memory on pathological
/// input with no whitespace.
const HARD_CAP: usize = 4096;
/// Idle timeout: flush after this long without a keystroke.
const IDLE: Duration = Duration::from_secs(2);

pub fn run(rx: Receiver<Ev>, snapshot: SharedSnapshot, front: SharedFrontApp) {
    let conn = db::open().expect("open database");
    let bpe = tiktoken_rs::o200k_base().expect("load tokenizer");
    let mut buf = String::new();

    // Publish initial state so the popover isn't empty at launch.
    refresh(&conn, &snapshot);

    loop {
        match rx.recv_timeout(IDLE) {
            Ok(Ev::Text(s)) => {
                buf.push_str(&s);
                if buf.len() >= HARD_CAP {
                    flush(&mut buf, true, &bpe, &conn, &front, &snapshot);
                } else if buf.len() >= FLUSH_CHARS {
                    flush(&mut buf, false, &bpe, &conn, &front, &snapshot);
                }
            }
            Ok(Ev::Backspace) => {
                buf.pop();
            }
            Ok(Ev::Newline) => {
                buf.push('\n');
                flush(&mut buf, true, &bpe, &conn, &front, &snapshot);
            }
            Ok(Ev::Boundary) => {
                flush(&mut buf, true, &bpe, &conn, &front, &snapshot);
            }
            Err(RecvTimeoutError::Timeout) => {
                flush(&mut buf, true, &bpe, &conn, &front, &snapshot);
                // Recompute even when nothing flushed, so the popover reflects
                // hour/day rollovers while idle.
                refresh(&conn, &snapshot);
            }
            Err(RecvTimeoutError::Disconnected) => break,
        }
    }
}

/// Flush buffered text. When `full`, flush everything; otherwise flush only up
/// to the last whitespace and keep the trailing partial word buffered (avoids
/// splitting a word across two flushes and double-counting it).
fn flush(
    buf: &mut String,
    full: bool,
    bpe: &CoreBPE,
    conn: &Connection,
    front: &SharedFrontApp,
    snapshot: &SharedSnapshot,
) {
    if buf.is_empty() {
        return;
    }

    let text: String = if full {
        std::mem::take(buf)
    } else {
        match buf.rfind(char::is_whitespace) {
            Some(idx) => {
                // byte index just past that whitespace char (a char boundary)
                let split = idx + buf[idx..].chars().next().unwrap().len_utf8();
                buf.drain(..split).collect()
            }
            // No boundary yet: wait for more input.
            None => return,
        }
    };

    if text.trim().is_empty() {
        return;
    }

    let words = text.split_whitespace().count() as i64;
    let chars = text.chars().filter(|c| !c.is_control()).count() as i64;
    let tokens = bpe.encode_ordinary(&text).len() as i64;

    let app = front
        .lock()
        .ok()
        .map(|g| g.clone())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "unknown".to_string());

    if let Err(e) = db::upsert(conn, db::current_hour(), &app, words, tokens, chars) {
        eprintln!("word-counter: db upsert failed: {e}");
    }
    refresh(conn, snapshot);
}

/// Recompute the dashboard snapshot from the database.
fn refresh(conn: &Connection, snapshot: &SharedSnapshot) {
    if let Ok(s) = db::snapshot(conn) {
        if let Ok(mut g) = snapshot.lock() {
            *g = s;
        }
    }
}
