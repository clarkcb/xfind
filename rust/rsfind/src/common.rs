use chrono::NaiveDate;
use chrono::format::ParseResult;

// logging
pub fn log(message: &str) {
    println!("{}", message);
}

pub fn date_from_string(s: &str) -> ParseResult<NaiveDate> {
    NaiveDate::parse_from_str(s, "%Y-%m-%d")
}
pub fn timestamp_from_date_string(s: &str) -> ParseResult<i64> {
    date_from_string(s).map(|d| d.and_hms_opt(0, 0, 0).unwrap().timestamp())
}
