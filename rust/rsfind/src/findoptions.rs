use core::slice::Iter;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::{fs, io};

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::common::{log, timestamp_from_date_string};
use crate::config::Config;
use crate::filetypes::FileTypes;
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
use crate::findsettings::FindSettings;
use crate::sortby::sort_by_from_name;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct FindOption {
    long: String,
    short: Option<String>,
    desc: String,
}

type BoolAction = Box<dyn Fn(bool, &mut FindSettings) -> Result<(), FindError>>;
type StringAction = Box<dyn Fn(&str, &mut FindSettings) -> Result<(), FindError>>;
type IntAction = Box<dyn Fn(i32, &mut FindSettings) -> Result<(), FindError>>;
type LongAction = Box<dyn Fn(u64, &mut FindSettings) -> Result<(), FindError>>;

pub struct FindOptions {
    pub find_options: Vec<FindOption>,
    pub version: String,
    pub bool_action_map: HashMap<String, BoolAction>,
    pub string_action_map: HashMap<String, StringAction>,
    pub int_action_map: HashMap<String, IntAction>,
    pub long_action_map: HashMap<String, LongAction>,
    pub long_arg_map: HashMap<String, String>,
}

#[derive(Serialize, Deserialize)]
pub struct JsonFindOptions {
    pub findoptions: Vec<FindOption>,
}

impl FindOptions {
    pub fn new() -> Result<FindOptions, FindError> {
        let config = Config::new();
        let contents: String = match fs::read_to_string(config.find_options_path) {
            Ok(contents) => contents,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        let jso: JsonFindOptions = match serde_json::from_str(&contents) {
            Ok(deserialized) => deserialized,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        Ok(FindOptions {
            find_options: jso.findoptions.clone(),
            version: config.version.clone(),
            bool_action_map: get_bool_action_map(),
            string_action_map: get_string_action_map(),
            int_action_map: get_int_action_map(),
            long_action_map: get_long_action_map(),
            long_arg_map: get_long_arg_map(&jso.findoptions),
        })
    }

    // fn get_long_map(&self) -> HashMap<String, String> {
    //     let mut map = HashMap::new();
    //     for so in self.find_options.iter() {
    //         map.insert(so.long.to_string(), so.long.to_string());
    //         if so.short.is_some() {
    //             map.insert(so.short.as_ref().unwrap().to_string(), so.long.to_string());
    //         }
    //     }
    //     map
    // }

    fn apply_bool_arg(
        &self,
        arg_name: &str,
        b: bool,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match self.bool_action_map.get(arg_name) {
            Some(arg_fn) => match arg_fn(b, settings) {
                Ok(_) => Ok(()),
                Err(error) => Err(error),
            },
            None => {
                Err(FindError::new(
                    format!("Invalid option: {}", arg_name).as_str(),
                ))
            }
        }
    }

    fn apply_string_arg(
        &self,
        arg_name: &str,
        s: &str,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match self.string_action_map.get(arg_name) {
            Some(arg_fn) => match arg_fn(&s, settings) {
                Ok(_) => Ok(()),
                Err(error) => Err(error),
            },
            None => {
                Err(FindError::new(
                    format!("Invalid option: {}", arg_name).as_str(),
                ))
            }
        }
    }

    fn apply_int_arg(
        &self,
        arg_name: &str,
        i: i32,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match self.int_action_map.get(arg_name) {
            Some(arg_fn) => match arg_fn(i, settings) {
                Ok(_) => Ok(()),
                Err(error) => Err(error),
            },
            None => {
                Err(FindError::new(
                    format!("Invalid option: {}", arg_name).as_str(),
                ))
            }
        }
    }

    fn apply_long_arg(
        &self,
        arg_name: &str,
        l: u64,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match self.long_action_map.get(arg_name) {
            Some(arg_fn) => match arg_fn(l, settings) {
                Ok(_) => Ok(()),
                Err(error) => Err(error),
            },
            None => {
                Err(FindError::new(
                    format!("Invalid option: {}", arg_name).as_str(),
                ))
            }
        }
    }

    fn settings_from_name_value(
        &self,
        name: &String,
        value: &Value,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        if self.bool_action_map.contains_key(name) {
            if value.is_boolean() {
                let b = value.as_bool().unwrap();
                if let Err(error) = self.apply_bool_arg(name, b, settings) {
                    return Err(error);
                }
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else if self.string_action_map.contains_key(name) {
            if value.is_string() {
                let s = value.as_str().unwrap();
                if let Err(error) = self.apply_string_arg(name, s, settings) {
                    return Err(error);
                }
            } else if value.is_array() {
                let array = value.as_array().unwrap();
                for v in array.iter() {
                    if let Err(error) = self.settings_from_name_value(name, &v, settings) {
                        return Err(error);
                    }
                }
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else if self.int_action_map.contains_key(name) {
            if value.is_number() {
                let l = value.as_i64().unwrap();
                match i32::try_from(l) {
                    Ok(i) => {
                        if let Err(error) = self.apply_int_arg(name, i, settings) {
                            return Err(error);
                        }
                    }
                    Err(error) => return Err(FindError::new(&error.to_string())),
                }
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else if self.long_action_map.contains_key(name) {
            if value.is_number() {
                let l = value.as_u64().unwrap();
                if let Err(error) = self.apply_long_arg(name, l, settings) {
                    return Err(error);
                }
            } else {
                return Err(FindError::new(&format!("Invalid value for option: {}", name)));
            }
        } else {
            return Err(FindError::new(&format!("Invalid option: {}", name)));
        }
        Ok(())
    }

    fn settings_from_value(
        &self,
        value: &Value,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match value {
            Value::Object(obj) => {
                let mut keys = obj.keys().into_iter().collect::<Vec<&String>>();
                keys.sort_unstable();
                for key in keys {
                    if !self.long_arg_map.contains_key(key) {
                        return Err(FindError::new(
                            format!("Invalid option: {}", key).as_str()
                        ))
                    }
                }
                for (s, v) in obj.iter() {
                    if let Err(error) = self.settings_from_name_value(&s, &v, settings) {
                        return Err(error);
                    }
                }
            },
            _ => {}
        }
        Ok(())
    }

    pub fn update_settings_from_json(&self, settings: &mut FindSettings, json_string: &str) -> Result<(), FindError> {
        match serde_json::from_str(json_string) {
            Ok(value) => self.settings_from_value(&value, settings),
            Err(_error) => Err(FindError::new("Unable to parse JSON")),
        }
    }

    pub fn settings_from_json(&self, json_string: &str) -> Result<FindSettings, FindError> {
        let mut settings = FindSettings::default();
        match self.update_settings_from_json(&mut settings, json_string) {
            Ok(()) => Ok(settings),
            Err(error) => Err(FindError::new(&error.to_string())),
        }
    }

    pub fn update_settings_from_file(&self, settings: &mut FindSettings, json_file: &str) -> Result<(), FindError> {
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
                Ok(json) => match self.update_settings_from_json(settings, &json) {
                    Ok(()) => Ok(()),
                    Err(error) => {
                        if error.description.eq("Unable to parse JSON") {
                            Err(FindError::new(
                                format!("Unable to parse JSON in settings file: {}", &json_file).as_str()))
                        } else {
                            Err(error)
                        }
                    },
                },
                Err(error) => Err(FindError::new(&error.to_string())),
            }
        } else {
            Err(FindError::new(
                format!("Invalid settings file (must be JSON): {}", &json_file).as_str()))
        }
    }

    pub fn settings_from_file(&self, json_file: &str) -> Result<FindSettings, FindError> {
        let mut settings = FindSettings::default();
        match self.update_settings_from_file(&mut settings, json_file) {
            Ok(()) => Ok(settings),
            Err(error) => Err(error),
        }
    }

    pub fn update_settings_from_args(
        &self,
        settings: &mut FindSettings,
        mut args: Iter<String>,
    ) -> Result<(), FindError> {

        loop {
            if settings.print_usage() || settings.print_version() {
                return Ok(());
            }
            match args.next() {
                // if it ends with rsfind, it's the executable arg, skip it
                Some(next_arg) if next_arg.ends_with("rsfind") => {},
                Some(next_arg) if next_arg.starts_with("-") => {
                    let arg = next_arg.trim_start_matches('-');
                    match self.long_arg_map.get(arg) {
                        Some(long_arg) if self.bool_action_map.contains_key(long_arg) => {
                            if let Err(error) = self.apply_bool_arg(long_arg, true, settings) {
                                return Err(error);
                            }
                        },
                        Some(long_arg) => match args.next() {
                            Some(arg_val) => {
                                if self.string_action_map.contains_key(long_arg) {
                                    if let Err(error) = self.apply_string_arg(long_arg, &arg_val, settings) {
                                        return Err(error);
                                    }
                                } else if self.int_action_map.contains_key(long_arg) {
                                    let i = arg_val.parse::<i32>().unwrap_or(0);
                                    if let Err(error) = self.apply_int_arg(long_arg, i, settings) {
                                        return Err(error);
                                    }
                                } else if self.long_action_map.contains_key(long_arg) {
                                    let l = arg_val.parse::<u64>().unwrap_or(0);
                                    if let Err(error) = self.apply_long_arg(long_arg, l, settings) {
                                        return Err(error);
                                    }
                                } else if long_arg == "settings-file" {
                                    if let Err(error) = self.update_settings_from_file(settings, &arg_val) {
                                        return Err(error);
                                    }
                                } else {
                                    return Err(FindError::new(
                                        format!("Invalid option: {}", &arg).as_str(),
                                    ))
                                }
                            }
                            None => {
                                return Err(FindError::new(
                                    format!("Missing value for option {}", &next_arg).as_str(),
                                ));
                            }
                        },
                        _ => {
                            return Err(FindError::new(
                                format!("Invalid option: {}", &arg).as_str(),
                            ))
                        }
                    }
                }
                Some(next_arg) => {
                    if let Err(error) = self.apply_string_arg("path", &next_arg, settings) {
                        return Err(error);
                    }
                },
                None => break,
            }
        }

        Ok(())
    }

    pub fn settings_from_args(
        &self,
        args: Iter<String>,
    ) -> Result<FindSettings, FindError> {
        let mut settings = FindSettings::default();
        settings.set_print_files(true); // default to true when running from main
        match self.update_settings_from_args(&mut settings, args) {
            Ok(()) => Ok(settings),
            Err(error) => Err(error),
        }
    }

    fn get_sort_opt_map(&self) -> HashMap<String, &FindOption> {
        let mut map = HashMap::with_capacity(self.find_options.len());
        for so in self.find_options.iter() {
            let sort_key = match &so.short {
                Some(short) => String::from(format!("{}@{}", short.to_ascii_lowercase(), &so.long)),
                None => String::from(&so.long),
            };
            map.insert(sort_key, so);
        }
        map
    }

    fn get_usage_string(&self) -> String {
        let mut usage = String::from("\nUsage:\n rsfind [options] <path> [<path> ...]");
        usage.push_str("\n\nOptions:\n");
        let sort_opt_map = self.get_sort_opt_map();
        let mut sort_keys: Vec<String> = Vec::with_capacity(self.find_options.len());
        for key in sort_opt_map.keys() {
            sort_keys.push(key.clone());
        }
        let mut maxlen: usize = 0;
        for so in self.find_options.iter() {
            let len = match &so.short {
                Some(_) => so.long.len() + 4,
                None => so.long.len() + 2,
            };
            if len > maxlen {
                maxlen = len;
            }
        }

        sort_keys.sort_unstable();
        for sort_key in sort_keys.iter() {
            let so = sort_opt_map.get(sort_key).unwrap();
            let opt_string = match &so.short {
                Some(short) => String::from(format!(" -{},--{}", short, &so.long)),
                None => String::from(format!(" --{}", &so.long)),
            };
            let opt_string = format!("{:maxlen$}", opt_string.as_str(), maxlen = maxlen + 1);
            usage.push_str(opt_string.as_str());
            usage.push_str("  ");
            usage.push_str(so.desc.as_str());
            usage.push_str("\n");
        }
        usage
    }

    pub fn print_usage(&self) {
        let usage = self.get_usage_string();
        log(format!("{}", usage).as_str());
    }

    pub fn print_version(&self) {
        log(format!("xfind version {}", self.version).as_str());
    }
}

fn get_bool_action_map() -> HashMap<String, BoolAction> {
    let mut bool_action_map: HashMap<String, BoolAction> = HashMap::with_capacity(21);
    bool_action_map.insert(
        "archivesonly".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_archives_only(b))),
    );
    bool_action_map.insert(
        "debug".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_debug(b))),
    );
    bool_action_map.insert(
        "excludearchives".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_include_archives(!b))),
    );
    bool_action_map.insert(
        "excludehidden".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_include_hidden(!b))),
    );
    bool_action_map.insert(
        "followsymlinks".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_follow_symlinks(b))),
    );
    bool_action_map.insert(
        "help".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_print_usage(b))),
    );
    bool_action_map.insert(
        "includearchives".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_include_archives(b))),
    );
    bool_action_map.insert(
        "includehidden".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_include_hidden(b))),
    );
    bool_action_map.insert(
        "nofollowsymlinks".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_follow_symlinks(!b))),
    );
    bool_action_map.insert(
        "noprintdirs".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_print_dirs(!b))),
    );
    bool_action_map.insert(
        "noprintfiles".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_print_files(!b))),
    );
    bool_action_map.insert(
        "norecursive".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_recursive(!b))),
    );
    bool_action_map.insert(
        "printdirs".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_print_dirs(b))),
    );
    bool_action_map.insert(
        "printfiles".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_print_files(b))),
    );
    bool_action_map.insert(
        "recursive".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_recursive(b))),
    );
    bool_action_map.insert(
        "sort-ascending".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_sort_descending(!b))),
    );
    bool_action_map.insert(
        "sort-caseinsensitive".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_sort_case_insensitive(b))),
    );
    bool_action_map.insert(
        "sort-casesensitive".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_sort_case_insensitive(!b))),
    );
    bool_action_map.insert(
        "sort-descending".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_sort_descending(b))),
    );
    bool_action_map.insert(
        "verbose".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_verbose(b))),
    );
    bool_action_map.insert(
        "version".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_print_version(b))),
    );
    bool_action_map
}

fn get_string_action_map() -> HashMap<String, StringAction> {
    let mut string_action_map: HashMap<String, StringAction> = HashMap::with_capacity(16);
    string_action_map.insert(
        "in-archiveext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_archive_extension(s.to_string()))
        }),
    );
    string_action_map.insert(
        "in-archivefilepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_archive_file_pattern(s.to_string()))
        }),
    );
    string_action_map.insert(
        "in-dirpattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_dir_pattern(s.to_string()))
        }),
    );
    string_action_map.insert(
        "in-ext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_extension(s.to_string()))
        }),
    );
    string_action_map.insert(
        "in-filepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_file_pattern(s.to_string()))
        }),
    );
    string_action_map.insert(
        "in-filetype".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            let file_type = FileTypes::file_type_for_name(&s.to_string());
            Ok(settings.add_in_file_type(file_type))
        }),
    );
    string_action_map.insert(
        "maxlastmod".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            let res = timestamp_from_date_string(s);
            match res {
                Ok(t) => {
                    settings.set_max_last_mod(t as u64);
                    Ok(())
                },
                Err(_) => {
                    res.map(|_t| ()).map_err(|_e| FindError {description: String::from("Unable to get timestamp from string")})
                }
            }
        }),
    );
    string_action_map.insert(
        "minlastmod".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            let res = timestamp_from_date_string(s);
            match res {
                Ok(t) => {
                    settings.set_min_last_mod(t as u64);
                    Ok(())
                },
                Err(_) => {
                    res.map(|_t| ()).map_err(|_e| FindError {description: String::from("Unable to get timestamp from string")})
                }
            }
        }),
    );
    string_action_map.insert(
        "out-archiveext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_archive_extension(s.to_string()))
        }),
    );
    string_action_map.insert(
        "out-archivefilepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_archive_file_pattern(s.to_string()))
        }),
    );
    string_action_map.insert(
        "out-dirpattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_dir_pattern(s.to_string()))
        }),
    );
    string_action_map.insert(
        "out-ext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_extension(s.to_string()))
        }),
    );
    string_action_map.insert(
        "out-filepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_file_pattern(s.to_string()))
        }),
    );
    string_action_map.insert(
        "out-filetype".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            let file_type = FileTypes::file_type_for_name(&s.to_string());
            Ok(settings.add_out_file_type(file_type))
        }),
    );
    string_action_map.insert(
        "path".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_path(s.to_string()))
        }),
    );
    string_action_map.insert(
        "sort-by".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.set_sort_by(sort_by_from_name(s)))
        }),
    );
    string_action_map
}

fn get_int_action_map() -> HashMap<String, IntAction> {
    let mut int_action_map: HashMap<String, IntAction> = HashMap::with_capacity(2);
    int_action_map.insert(
        "maxdepth".to_string(),
        Box::new(|i: i32, settings: &mut FindSettings| {
            Ok(settings.set_max_depth(i))
        }),
    );
    int_action_map.insert(
        "mindepth".to_string(),
        Box::new(|i: i32, settings: &mut FindSettings| {
            Ok(settings.set_min_depth(i))
        }),
    );
    int_action_map
}

fn get_long_action_map() -> HashMap<String, LongAction> {
    let mut long_action_map: HashMap<String, LongAction> = HashMap::with_capacity(2);
    long_action_map.insert(
        "maxsize".to_string(),
        Box::new(|l: u64, settings: &mut FindSettings| {
            Ok(settings.set_max_size(l))
        }),
    );
    long_action_map.insert(
        "minsize".to_string(),
        Box::new(|l: u64, settings: &mut FindSettings| {
            Ok(settings.set_min_size(l))
        }),
    );
    long_action_map
}

fn get_long_arg_map(options: &Vec<FindOption>) -> HashMap<String, String> {
    let mut map = HashMap::new();
    map.insert("path".to_string(), "path".to_string());
    for so in options.iter() {
        map.insert(so.long.to_string(), so.long.to_string());
        if so.short.is_some() {
            map.insert(so.short.as_ref().unwrap().to_string(), so.long.to_string());
        }
    }
    map
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::process;

    use crate::filetypes::FileType;

    use super::*;

    #[test]
    fn test_settings_from_args() {
        let options = match FindOptions::new() {
            Ok(options) => options,
            Err(error) => {
                log(&error.to_string());
                assert!(false);
                process::exit(1);
            }
        };
        assert!(!options.find_options.is_empty());

        let args: Vec<String> = vec![
            "rsfind", "-x", "php,rs", "-D", "debug", "-f", "find", "-t",
            "code", "--debug", ".",
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();
        let result = options.settings_from_args(args.iter());
        assert!(result.is_ok());
        let settings = result.ok().unwrap();

        // verify these defaults
        assert_eq!(settings.include_archives(), false);
        assert_eq!(settings.print_dirs(), false);
        assert_eq!(settings.print_files(), true);

        assert_eq!(settings.in_extensions().len(), 2);
        assert_eq!(settings.in_extensions()[0], String::from("php"));
        assert_eq!(settings.in_extensions()[1], String::from("rs"));
        assert_eq!(settings.out_dir_patterns().len(), 1);
        assert_eq!(
            settings.out_dir_patterns()[0].to_string(),
            String::from("debug")
        );
        assert_eq!(settings.in_file_patterns().len(), 1);
        assert_eq!(
            settings.in_file_patterns()[0].to_string(),
            String::from("find")
        );
        assert_eq!(settings.in_file_types().len(), 1);
        assert_eq!(settings.in_file_types()[0], FileType::Code);
        assert!(settings.debug());
        assert!(settings.verbose());
        assert_eq!(settings.paths().len(), 1);
        assert_eq!(settings.paths()[0], String::from("."));
    }

    #[test]
    fn test_settings_from_json() {
        let options = match FindOptions::new() {
            Ok(options) => options,
            Err(error) => {
                log(&error.to_string());
                assert!(false);
                process::exit(1);
            }
        };
        assert!(!options.find_options.is_empty());

        let json = r#"
            {
              "debug": true,
              "followsymlinks": true,
              "in-ext": ["js","ts"],
              "includehidden": true,
              "out-dirpattern": "node_module",
              "out-filepattern": ["temp"],
              "path": "~/src/xfind/"
            }"#;

        match options.settings_from_json(&json.to_string()) {
            Ok(settings) => {
                assert!(settings.debug());
                assert!(settings.follow_symlinks());
                assert_eq!(settings.in_extensions().len(), 2);
                assert_eq!(settings.in_extensions()[0], String::from("js"));
                assert_eq!(settings.in_extensions()[1], String::from("ts"));
                assert!(settings.include_hidden());
                assert_eq!(settings.out_dir_patterns().len(), 1);
                assert_eq!(
                    settings.out_dir_patterns()[0].to_string(),
                    String::from("node_module")
                );
                assert_eq!(settings.out_file_patterns().len(), 1);
                assert_eq!(
                    settings.out_file_patterns()[0].to_string(),
                    String::from("temp")
                );
                assert_eq!(settings.paths().len(), 1);
                assert_eq!(settings.paths()[0], String::from("~/src/xfind/"));
                assert!(settings.verbose());
            },
            Err(error) => {
                log(&error.to_string());
                assert!(false)
            }
        }
    }

    // NOTE: this test is unreliable because the settings file can change, should probably deactivate
    #[test]
    fn test_settings_from_file() {
        let options = match FindOptions::new() {
            Ok(options) => options,
            Err(error) => {
                log(&error.to_string());
                assert!(false);
                process::exit(1);
            }
        };
        assert!(!options.find_options.is_empty());

        // let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let config = Config::new();
        let path = Path::new(config.shared_path.as_str()).join("settings.json");
        let settings_file = path.to_str().unwrap();

        let args: Vec<&str> = vec!["rsfind", "--settings-file", &settings_file];
        let args: Vec<String> = args.into_iter().map(|a| a.to_string()).collect();
        match options.settings_from_args(args.iter()) {
            Ok(settings) => {
                assert!(!settings.include_hidden());
                assert_eq!(settings.in_extensions().len(), 2);
                assert_eq!(settings.in_extensions()[0], String::from("js"));
                assert_eq!(settings.in_extensions()[1], String::from("ts"));
                assert!(settings.out_dir_patterns().len() > 0);
                assert!(settings.paths().len() > 0);
                assert!(settings.print_dirs());
                assert!(settings.print_files());
            },
            Err(error) => {
                log(&error.to_string());
                assert!(false)
            }
        }
    }
}
