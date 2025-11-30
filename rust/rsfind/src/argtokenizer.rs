use std::collections::HashMap;
use std::{fs, io};
use std::slice::Iter;
use regex::Regex;
use crate::finderror::FindError;

use once_cell::sync::Lazy;
use serde_json::{Map, Value};
use crate::fileutil::FileUtil;


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ArgTokenType {
    Unknown,
    Bool,
    String,
    Int,
    Long,
}

#[derive(Clone, Debug)]
pub enum ArgToken {
    Bool { name: String, value: bool },
    String { name: String, value: String },
    Int { name: String, value: i32 },
    Long { name: String, value: i64 },
}

#[derive(Clone, Debug)]
pub struct ArgOption {
    pub long: String,
    pub short: Option<String>,
    pub desc: String,
    pub arg_type: ArgTokenType,
}

static LONG_ARG_WITH_VAL_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new("^--([a-zA-Z0-9-]+)=(.+)$").unwrap()
});
static LONG_ARG_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new("^--([a-zA-Z0-9-]+)$").unwrap()
});
static SHORT_ARGS_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new("^-([a-zA-Z0-9-]{2,})$").unwrap()
});
static SHORT_ARG_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new("^-([a-zA-Z0-9-])$").unwrap()
});

pub struct ArgTokenizer {
    pub bool_map: HashMap<String, String>,
    pub string_map: HashMap<String, String>,
    pub int_map: HashMap<String, String>,
    pub long_map: HashMap<String, String>,
}

impl ArgTokenizer {
    pub fn new(options: &Vec<ArgOption>) -> ArgTokenizer {
        let mut bool_map: HashMap<String, String> = HashMap::new();
        let mut string_map: HashMap<String, String> = HashMap::new();
        let mut int_map: HashMap<String, String> = HashMap::new();
        let mut long_map: HashMap<String, String> = HashMap::new();
        for o in options.iter() {
            if o.arg_type == ArgTokenType::Bool {
                bool_map.insert(o.long.to_string(), o.long.to_string());
                if o.short.is_some() {
                    bool_map.insert(o.short.as_ref().unwrap().to_string(), o.long.to_string());
                }
            } else if o.arg_type == ArgTokenType::String {
                string_map.insert(o.long.to_string(), o.long.to_string());
                if o.short.is_some() {
                    string_map.insert(o.short.as_ref().unwrap().to_string(), o.long.to_string());
                }
            } else if o.arg_type == ArgTokenType::Int {
                int_map.insert(o.long.to_string(), o.long.to_string());
                if o.short.is_some() {
                    int_map.insert(o.short.as_ref().unwrap().to_string(), o.long.to_string());
                }
            } else if o.arg_type == ArgTokenType::Long {
                long_map.insert(o.long.to_string(), o.long.to_string());
                if o.short.is_some() {
                    long_map.insert(o.short.as_ref().unwrap().to_string(), o.long.to_string());
                }
            }
        }

        ArgTokenizer {
            bool_map,
            string_map,
            int_map,
            long_map,
        }
    }

    fn rec_tokenize_args(&self, mut args: Iter<String>, mut arg_tokens: Vec<ArgToken>) -> Result<Vec<ArgToken>, FindError> {
        match args.next() {
            Some(next_arg) if LONG_ARG_WITH_VAL_REGEX.is_match(&next_arg) => {
                let caps = LONG_ARG_WITH_VAL_REGEX.captures(&next_arg).unwrap();
                let new_arg = format!("--{}", caps.get(1).unwrap().as_str());
                let new_val = String::from(caps.get(2).unwrap().as_str());
                let new_args = vec![new_arg, new_val];
                let next_args = new_args.iter().chain(args).cloned().collect::<Vec<String>>();
                self.rec_tokenize_args(next_args.iter(), arg_tokens)
            },
            Some(next_arg) if LONG_ARG_REGEX.is_match(&next_arg) => {
                // Handle long arg - this is where most of the processing happens
                let long_arg = next_arg.trim_start_matches('-');
                if self.bool_map.contains_key(long_arg) {
                    arg_tokens.push(ArgToken::Bool { name: long_arg.into(), value: true });
                    self.rec_tokenize_args(args, arg_tokens)
                } else {
                    match args.next() {
                        Some(arg_val) => {
                            if self.string_map.contains_key(long_arg) {
                                arg_tokens.push(ArgToken::String { name: long_arg.into(), value: arg_val.into() });
                                self.rec_tokenize_args(args, arg_tokens)
                            } else if self.int_map.contains_key(long_arg) {
                                let i = arg_val.parse::<i32>().unwrap_or(0);
                                arg_tokens.push(ArgToken::Int { name: long_arg.into(), value: i });
                                self.rec_tokenize_args(args, arg_tokens)
                            } else if self.long_map.contains_key(long_arg) {
                                let l = arg_val.parse::<i64>().unwrap_or(0);
                                arg_tokens.push(ArgToken::Long { name: long_arg.into(), value: l });
                                self.rec_tokenize_args(args, arg_tokens)
                            } else if long_arg == "settings-file" {
                                arg_tokens.push(ArgToken::String { name: long_arg.into(), value: arg_val.into() });
                                self.rec_tokenize_args(args, arg_tokens)
                            } else {
                                Err(FindError::new(
                                    format!("Invalid option: {}", &long_arg).as_str(),
                                ))
                            }
                        }
                        None => {
                            Err(FindError::new(
                                format!("Missing value for option {}", &next_arg).as_str(),
                            ))
                        }
                    }
                }
            },
            Some(next_arg) if SHORT_ARGS_REGEX.is_match(&next_arg) => {
                // Handle short args
                let next_arg = next_arg.trim_start_matches('-');
                let new_args = next_arg.chars().map(|c| format!("-{}", c)).collect::<Vec<String>>();
                let next_args = new_args.iter().chain(args).cloned().collect::<Vec<String>>();
                self.rec_tokenize_args(next_args.iter(), arg_tokens)
            },
            Some(next_arg) if SHORT_ARG_REGEX.is_match(&next_arg) => {
                // Handle short arg
                let next_arg = next_arg.trim_start_matches('-');
                let mut opt_long_arg: Option<String> = None;
                if self.bool_map.contains_key(next_arg) {
                    opt_long_arg = self.bool_map.get(next_arg).map(|s| s.to_string());
                } else if self.string_map.contains_key(next_arg) {
                    opt_long_arg = self.string_map.get(next_arg).map(|s| s.to_string());
                } else if self.int_map.contains_key(next_arg) {
                    opt_long_arg = self.int_map.get(next_arg).map(|s| s.to_string());
                } else if self.long_map.contains_key(next_arg) {
                    opt_long_arg = self.long_map.get(next_arg).map(|s| s.to_string());
                }
                match opt_long_arg {
                    Some(long_arg) => {
                        let long_arg = format!("--{}", long_arg);
                        let new_args = vec![long_arg];
                        let next_args = new_args.iter().chain(args).cloned().collect::<Vec<String>>();
                        self.rec_tokenize_args(next_args.iter(), arg_tokens)
                    },
                    None => {
                        Err(FindError::new(
                            format!("Invalid option: {}", &next_arg).as_str(),
                        ))
                    }
                }
            },
            Some(next_arg) => {
                arg_tokens.push(ArgToken::String { name: "path".into(), value: next_arg.into() });
                self.rec_tokenize_args(args, arg_tokens)
            },
            None => Ok(arg_tokens)
        }
    }

    pub fn tokenize_args(&self, args: Iter<String>) -> Result<Vec<ArgToken>, FindError> {
        self.rec_tokenize_args(args, Vec::new())
    }

    fn tokenize_name_value(&self, name: &String, value: &Value) -> Result<Vec<ArgToken>, FindError> {
        let mut arg_tokens: Vec<ArgToken> = Vec::new();
        if self.bool_map.contains_key(name) {
            if value.is_boolean() {
                let b = value.as_bool().unwrap();
                arg_tokens.push(ArgToken::Bool { name: name.into(), value: b });
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else if self.string_map.contains_key(name) {
            if value.is_string() {
                let s = value.as_str().unwrap();
                arg_tokens.push(ArgToken::String { name: name.into(), value: s.to_string() });
            } else if value.is_array() {
                let array = value.as_array().unwrap();
                for v in array.iter() {
                    if v.is_string() {
                        let s = v.as_str().unwrap();
                        arg_tokens.push(ArgToken::String { name: name.into(), value: s.to_string() });
                    } else {
                        return Err(FindError::new(&format!("Invalid value for option: {}", name)));
                    }
                }
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else if self.int_map.contains_key(name) {
            if value.is_number() {
                let l = value.as_i64().unwrap();
                match i32::try_from(l) {
                    Ok(i) => {
                        arg_tokens.push(ArgToken::Int { name: name.into(), value: i });
                    }
                    Err(error) => return Err(FindError::new(&error.to_string())),
                }
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else if self.long_map.contains_key(name) {
            if value.is_number() {
                let l = value.as_i64().unwrap();
                arg_tokens.push(ArgToken::Long { name: name.into(), value: l });
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else {
            return Err(FindError::new(&format!("Invalid option: {}", name)));
        }
        Ok(arg_tokens)
    }

    fn tokenize_json_map(&self, json_map: &Map<String, Value>) -> Result<Vec<ArgToken>, FindError> {
        let mut arg_tokens: Vec<ArgToken> = Vec::new();
        let mut keys = json_map.keys().into_iter().collect::<Vec<&String>>();
        keys.sort_unstable();
        for (s, v) in json_map.iter() {
            match self.tokenize_name_value(&s, &v) {
                Ok(tokens) => arg_tokens.extend(tokens),
                Err(error) => return Err(error),
            }
        }
        Ok(arg_tokens)
    }

    fn tokenize_json_value(&self, value: &Value) -> Result<Vec<ArgToken>, FindError> {
        match value {
            Value::Object(obj) => {
                self.tokenize_json_map(&obj)
            },
            _ => Err(FindError::new(
                "Invalid JSON type (not object)".to_string().as_str()
            ))
        }
    }

    pub fn tokenize_json(&self, json_string: &str) -> Result<Vec<ArgToken>, FindError> {
        match serde_json::from_str(json_string) {
            Ok(value) => self.tokenize_json_value(&value),
            Err(_error) => Err(FindError::new("Unable to parse JSON")),
        }
    }

    pub fn tokenize_file(&self, json_file: &str) -> Result<Vec<ArgToken>, FindError> {
        let expanded_path = FileUtil::expand_path_string(json_file);
        let metadata = fs::metadata(&expanded_path);
        if metadata.is_err() {
            return match metadata.err().unwrap().kind() {
                io::ErrorKind::NotFound => Err(FindError::new(
                    format!("Settings file not found: {}", &json_file).as_str())),
                io::ErrorKind::PermissionDenied => Err(FindError::new(
                    format!("Settings file not readable: {}", &json_file).as_str())),
                _ => {
                    Err(FindError::new(
                        "An unknown error occurred trying to read settings file"))
                }
            }
        }
        if json_file.ends_with(".json") {
            match fs::read_to_string(expanded_path) {
                Ok(json) => self.tokenize_json(json.as_str()),
                Err(error) => Err(FindError::new(&error.to_string())),
            }
        } else {
            Err(FindError::new(
                format!("Invalid settings file (must be JSON): {}", &json_file).as_str()))
        }
    }
}
