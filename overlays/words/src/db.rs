//! SQLite storage. One row per (hour, app) with running word/token/char
//! totals. No text content is ever stored.

use crate::events::{AppRaw, RangeData, SeriesPoint, Snapshot};
use chrono::{Datelike, Duration, Local, TimeZone, Timelike, Utc};
use rusqlite::{params, Connection};
use std::path::PathBuf;

const DAY: i64 = 86_400;

/// `~/Library/Application Support/word-counter/`
pub fn db_dir() -> PathBuf {
    let mut p = dirs::data_dir().unwrap_or_else(std::env::temp_dir);
    p.push("word-counter");
    p
}

pub fn db_path() -> PathBuf {
    let mut p = db_dir();
    p.push("counts.db");
    p
}

/// Open (creating if needed) the database and ensure the schema exists.
/// WAL mode lets other tools read concurrently while we write.
pub fn open() -> rusqlite::Result<Connection> {
    let _ = std::fs::create_dir_all(db_dir());
    let conn = Connection::open(db_path())?;
    let _ = conn.pragma_update(None, "journal_mode", "WAL");
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS counts (
            hour_start    INTEGER NOT NULL,  -- unix epoch seconds, truncated to the hour (UTC)
            app_bundle_id TEXT    NOT NULL,
            words         INTEGER NOT NULL DEFAULT 0,
            tokens        INTEGER NOT NULL DEFAULT 0,
            chars         INTEGER NOT NULL DEFAULT 0,
            PRIMARY KEY (hour_start, app_bundle_id)
        );",
    )?;
    Ok(conn)
}

/// Add counts to the (hour, app) bucket, creating it if absent.
pub fn upsert(
    conn: &Connection,
    hour: i64,
    app: &str,
    words: i64,
    tokens: i64,
    chars: i64,
) -> rusqlite::Result<()> {
    conn.execute(
        "INSERT INTO counts (hour_start, app_bundle_id, words, tokens, chars)
         VALUES (?1, ?2, ?3, ?4, ?5)
         ON CONFLICT(hour_start, app_bundle_id) DO UPDATE SET
             words  = words  + excluded.words,
             tokens = tokens + excluded.tokens,
             chars  = chars  + excluded.chars",
        params![hour, app, words, tokens, chars],
    )?;
    Ok(())
}

/// The hourly bucket key (UTC epoch) containing `now`.
pub fn current_hour() -> i64 {
    (Utc::now().timestamp() / 3600) * 3600
}

/// Unix epoch of local midnight today.
fn local_midnight_ts() -> i64 {
    let today = Local::now().date_naive();
    let midnight = today.and_hms_opt(0, 0, 0).unwrap();
    Local
        .from_local_datetime(&midnight)
        .single()
        .map(|dt| dt.timestamp())
        .unwrap_or(0)
}

/// Total (tokens, words) for buckets in `[start, end)`.
fn range_totals(conn: &Connection, start: i64, end: i64) -> rusqlite::Result<(i64, i64)> {
    conn.query_row(
        "SELECT COALESCE(SUM(tokens),0), COALESCE(SUM(words),0)
         FROM counts WHERE hour_start >= ?1 AND hour_start < ?2",
        params![start, end],
        |r| Ok((r.get(0)?, r.get(1)?)),
    )
}

/// Top apps by tokens for buckets in `[start, end)`.
fn range_apps(conn: &Connection, start: i64, end: i64) -> rusqlite::Result<Vec<AppRaw>> {
    let mut stmt = conn.prepare(
        "SELECT app_bundle_id, COALESCE(SUM(tokens),0), COALESCE(SUM(words),0)
         FROM counts WHERE hour_start >= ?1 AND hour_start < ?2
         GROUP BY app_bundle_id ORDER BY SUM(tokens) DESC LIMIT 8",
    )?;
    let rows = stmt.query_map(params![start, end], |r| {
        Ok(AppRaw {
            bundle_id: r.get(0)?,
            tokens: r.get(1)?,
            words: r.get(2)?,
        })
    })?;
    rows.collect()
}

/// Rows of (hour_start, tokens, words) in `[start, end)`.
fn range_rows(conn: &Connection, start: i64, end: i64) -> rusqlite::Result<Vec<(i64, i64, i64)>> {
    let mut stmt = conn.prepare(
        "SELECT hour_start, tokens, words FROM counts WHERE hour_start >= ?1 AND hour_start < ?2",
    )?;
    let rows = stmt.query_map(params![start, end], |r| {
        Ok((r.get::<_, i64>(0)?, r.get::<_, i64>(1)?, r.get::<_, i64>(2)?))
    })?;
    rows.collect()
}

/// Today: 24 bars, one per local hour.
fn today_series(conn: &Connection, midnight: i64) -> rusqlite::Result<Vec<SeriesPoint>> {
    let mut pts: Vec<SeriesPoint> = (0..24)
        .map(|h| SeriesPoint {
            label: h.to_string(),
            tokens: 0,
            words: 0,
        })
        .collect();
    for (hs, tok, wd) in range_rows(conn, midnight, midnight + DAY)? {
        let hour = Local
            .timestamp_opt(hs, 0)
            .single()
            .map(|d| d.hour() as usize)
            .unwrap_or(0);
        pts[hour].tokens += tok;
        pts[hour].words += wd;
    }
    Ok(pts)
}

/// Week/Month: one bar per day, `n_days` ending today. Weekday labels for a
/// week, day-of-month labels for a month.
fn daily_series(
    conn: &Connection,
    start: i64,
    n_days: i64,
    weekday_labels: bool,
) -> rusqlite::Result<Vec<SeriesPoint>> {
    let start_date = Local.timestamp_opt(start, 0).single().map(|d| d.date_naive());
    let mut pts: Vec<SeriesPoint> = (0..n_days)
        .map(|i| {
            let label = start_date
                .and_then(|d| d.checked_add_signed(Duration::days(i)))
                .map(|d| {
                    if weekday_labels {
                        // Mon, Tue, …
                        d.format("%a").to_string()
                    } else {
                        d.day().to_string()
                    }
                })
                .unwrap_or_default();
            SeriesPoint {
                label,
                tokens: 0,
                words: 0,
            }
        })
        .collect();
    for (hs, tok, wd) in range_rows(conn, start, start + n_days * DAY)? {
        let idx = ((hs - start) / DAY).clamp(0, n_days - 1) as usize;
        pts[idx].tokens += tok;
        pts[idx].words += wd;
    }
    Ok(pts)
}

/// Build the full dashboard snapshot for all three ranges.
pub fn snapshot(conn: &Connection) -> rusqlite::Result<Snapshot> {
    let midnight = local_midnight_ts();

    // Today.
    let (tt, tw) = range_totals(conn, midnight, midnight + DAY)?;
    let (yt, yw) = range_totals(conn, midnight - DAY, midnight)?;
    let (w7t, w7w) = range_totals(conn, midnight - 6 * DAY, midnight + DAY)?;
    let today = RangeData {
        tokens: tt,
        words: tw,
        prev_tokens: yt,
        prev_words: yw,
        avg_tokens: w7t / 7,
        avg_words: w7w / 7,
        series: today_series(conn, midnight)?,
        apps: range_apps(conn, midnight, midnight + DAY)?,
    };

    // Week: trailing 7 days including today vs the 7 before it.
    let wk_start = midnight - 6 * DAY;
    let (wt, ww) = range_totals(conn, wk_start, midnight + DAY)?;
    let (pwt, pww) = range_totals(conn, wk_start - 7 * DAY, wk_start)?;
    let week = RangeData {
        tokens: wt,
        words: ww,
        prev_tokens: pwt,
        prev_words: pww,
        avg_tokens: wt / 7,
        avg_words: ww / 7,
        series: daily_series(conn, wk_start, 7, true)?,
        apps: range_apps(conn, wk_start, midnight + DAY)?,
    };

    // Month: trailing 30 days including today vs the 30 before it.
    let mo_start = midnight - 29 * DAY;
    let (mt, mw) = range_totals(conn, mo_start, midnight + DAY)?;
    let (pmt, pmw) = range_totals(conn, mo_start - 30 * DAY, mo_start)?;
    let month = RangeData {
        tokens: mt,
        words: mw,
        prev_tokens: pmt,
        prev_words: pmw,
        avg_tokens: mt / 30,
        avg_words: mw / 30,
        series: daily_series(conn, mo_start, 30, false)?,
        apps: range_apps(conn, mo_start, midnight + DAY)?,
    };

    Ok(Snapshot { today, week, month })
}
