use chrono::NaiveDate;
use chrono::format::ParseResult;
use regex::Regex;

// logging
pub fn log(message: &str) {
    println!("{}", message);
}

pub fn log_err(message: &str) {
    eprintln!("ERROR: {}", message);
}

pub fn date_from_string(s: &str) -> ParseResult<NaiveDate> {
    NaiveDate::parse_from_str(s, "%Y-%m-%d")
}
pub fn timestamp_from_date_string(s: &str) -> ParseResult<i64> {
    date_from_string(s).map(|d| d.and_hms_opt(0, 0, 0).unwrap().and_utc().timestamp())
}

pub fn get_regex_vec_string(vec: &Vec<Regex>) -> String {
    let patterns: Vec<String> = vec.iter().map(|r| r.to_string()).collect();
    let mut s = String::from("[");
    let mut i = 0;
    for pattern in patterns.iter() {
        if i > 0 {
            s.push_str(", ");
        }
        s.push_str(format!("{:?}", pattern).as_str());
        i += 1;
    }
    s.push_str("]");
    s
}
