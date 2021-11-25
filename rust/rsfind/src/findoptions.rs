use core::slice::Iter;
use std::collections::HashMap;
use std::fs;

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::common::log;
use crate::config::Config;
use crate::filetypes::FileTypes;
use crate::finderror::FindError;
use crate::findsettings::FindSettings;

#[derive(Serialize, Deserialize, Debug)]
pub struct FindOption {
    long: String,
    short: Option<String>,
    desc: String,
}

type ArgAction = Box<dyn Fn(&str, &mut FindSettings) -> Result<(), FindError>>;
type FlagAction = Box<dyn Fn(bool, &mut FindSettings) -> Result<(), FindError>>;

pub struct FindOptions {
    pub findoptions: Vec<FindOption>,
    pub version: String,
    pub arg_map: HashMap<String, ArgAction>,
    pub flag_map: HashMap<String, FlagAction>,
}

#[derive(Serialize, Deserialize)]
pub struct JsonFindOptions {
    pub findoptions: Vec<FindOption>,
}

impl FindOptions {
    pub fn new() -> Result<FindOptions, FindError> {
        let config = Config::new();
        let contents: String = match fs::read_to_string(config.findoptions_path) {
            Ok(contents) => contents,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        let jso: JsonFindOptions = match serde_json::from_str(&contents) {
            Ok(deserialized) => deserialized,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        Ok(FindOptions {
            findoptions: jso.findoptions,
            version: config.version.clone(),
            arg_map: get_arg_map(),
            flag_map: get_flag_map(),
        })
    }

    fn get_long_map(&self) -> HashMap<String, String> {
        let mut map = HashMap::new();
        for so in self.findoptions.iter() {
            map.insert(so.long.to_string(), so.long.to_string());
            if so.short.is_some() {
                map.insert(so.short.as_ref().unwrap().to_string(), so.long.to_string());
            }
        }
        map
    }

    pub fn settings_from_file(&self, json_file: &str) -> Result<FindSettings, FindError> {
        match fs::read_to_string(json_file) {
            Ok(json) => self.settings_from_json(&json),
            Err(error) => Err(FindError::new(&error.to_string())),
        }
    }

    pub fn settings_from_json(&self, json_string: &str) -> Result<FindSettings, FindError> {
        let mut settings = FindSettings::default();
        settings.list_files = true; // default to true when running from main
        match serde_json::from_str(json_string) {
            Ok(value) => {
                if let Err(error) = self.settings_from_value(&value, &mut settings) {
                    return Err(error);
                }
            },
            Err(error) => return Err(FindError::new(&error.to_string())),
        }
        Ok(settings)
    }

    fn settings_from_name_value(
        &self,
        name: &String,
        value: &Value,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match value {
            Value::Array(values) => {
                for v in values.iter() {
                    if let Err(error) = self.settings_from_name_value(name, &v, settings) {
                        return Err(error);
                    }
                }
            },
            Value::Bool(b) => {
                if let Err(error) = self.apply_flag(name, *b, settings) {
                    return Err(error);
                }
            },
            Value::Number(n) => {
                let s: String = format!("{}", n);
                if let Err(error) = self.apply_arg(name, &s, settings) {
                    return Err(error);
                }
            },
            Value::Object(_) => {
                if let Err(error) = self.settings_from_value(value, settings) {
                    return Err(error);
                }
            },
            Value::String(s) => {
                if let Err(error) = self.apply_arg(name, &s, settings) {
                    return Err(error);
                }
            }
            _ => {}
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

    pub fn settings_from_args(
        &self,
        mut args: Iter<String>,
    ) -> Result<FindSettings, FindError> {
        args.next(); // the first arg is assumed to be the executable name/path
        let mut settings = FindSettings::default();
        settings.list_files = true; // default to true when running from main

        let long_map = self.get_long_map();

        loop {
            if settings.print_usage || settings.print_version {
                return Ok(settings);
            }
            match args.next() {
                Some(nextarg) if nextarg.starts_with("-") => {
                    let longarg = nextarg.trim_start_matches('-');
                    match long_map.get(longarg) {
                        Some(arg) if arg == "settings-file" => match args.next() {
                            Some(argval) => match self.settings_from_file(&argval) {
                                Ok(file_settings) => settings = file_settings,
                                Err(error) => return Err(error),
                            },
                            None => {
                                return Err(FindError::new(
                                    format!("Missing value for option {}", &nextarg).as_str(),
                                ));
                            }
                        },
                        Some(arg) if self.arg_map.contains_key(arg) => match args.next() {
                            Some(argval) => {
                                if let Err(error) = self.apply_arg(arg, &argval, &mut settings) {
                                    return Err(error);
                                }
                            },
                            None => {
                                return Err(FindError::new(
                                    format!("Missing value for option {}", &nextarg).as_str(),
                                ));
                            }
                        },
                        Some(arg) if self.flag_map.contains_key(arg) => {
                            if let Err(error) = self.apply_flag(arg, true, &mut settings) {
                                return Err(error);
                            }
                        },
                        _ => {
                            return Err(FindError::new(
                                format!("Invalid option: {}", &nextarg).as_str(),
                            ))
                        }
                    }
                },
                Some(nextarg) => {
                    // settings.startpath = String::from(nextarg);
                    if let Err(error) = self.apply_arg("path", &nextarg, &mut settings) {
                        return Err(error);
                    }
                },
                None => break,
            }
        }

        Ok(settings)
    }

    fn get_sort_opt_map(&self) -> HashMap<String, &FindOption> {
        let mut map = HashMap::with_capacity(self.findoptions.len());
        for so in self.findoptions.iter() {
            let sortkey = match &so.short {
                Some(short) => String::from(format!("{}@{}", short.to_ascii_lowercase(), &so.long)),
                None => String::from(&so.long),
            };
            map.insert(sortkey, so.clone());
        }
        map
    }

    fn get_usage_string(&self) -> String {
        let mut usage = String::from("\nUsage:\n rsfind [options] <path> [<path> ...]");
        usage.push_str("\n\nOptions:\n");
        let sort_opt_map = self.get_sort_opt_map();
        let mut sortkeys: Vec<String> = Vec::with_capacity(self.findoptions.len());
        for key in sort_opt_map.keys() {
            sortkeys.push(key.clone());
        }
        let mut maxlen: usize = 0;
        for so in self.findoptions.iter() {
            let len = match &so.short {
                Some(_) => so.long.len() + 4,
                None => so.long.len() + 2,
            };
            if len > maxlen {
                maxlen = len;
            }
        }

        sortkeys.sort_unstable();
        for sortkey in sortkeys.iter() {
            let so = sort_opt_map.get(sortkey).unwrap();
            let optstring = match &so.short {
                Some(short) => String::from(format!(" -{},--{}", short, &so.long)),
                None => String::from(format!(" --{}", &so.long)),
            };
            let optstring = format!("{:maxlen$}", optstring.as_str(), maxlen = maxlen + 1);
            usage.push_str(optstring.as_str());
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

    fn apply_arg(
        &self,
        argname: &str,
        s: &str,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match self.arg_map.get(argname) {
            Some(arg_fn) => match arg_fn(&s, settings) {
                Ok(_) => return Ok(()),
                Err(error) => return Err(error),
            },
            None => {
                return Err(FindError::new(
                    format!("Invalid option: {}", argname).as_str(),
                ))
            }
        }
    }

    fn apply_flag(
        &self,
        argname: &str,
        b: bool,
        settings: &mut FindSettings,
    ) -> Result<(), FindError> {
        match self.flag_map.get(argname) {
            Some(arg_fn) => match arg_fn(b, settings) {
                Ok(_) => return Ok(()),
                Err(error) => return Err(error),
            },
            None => {
                return Err(FindError::new(
                    format!("Invalid option: {}", argname).as_str(),
                ))
            }
        }
    }
}

fn get_arg_map() -> HashMap<String, ArgAction> {
    let mut arg_map: HashMap<String, ArgAction> = HashMap::with_capacity(23);
    arg_map.insert(
        "in-archiveext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_archive_extension(s.to_string()))
        }),
    );
    arg_map.insert(
        "in-archivefilepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_archive_file_pattern(s.to_string()))
        }),
    );
    arg_map.insert(
        "in-dirpattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_dir_pattern(s.to_string()))
        }),
    );
    arg_map.insert(
        "in-ext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_extension(s.to_string()))
        }),
    );
    arg_map.insert(
        "in-filepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_in_file_pattern(s.to_string()))
        }),
    );
    arg_map.insert(
        "in-filetype".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            let filetype = FileTypes::file_type_for_name(&s.to_string());
            Ok(settings.add_in_file_type(filetype))
        }),
    );
    arg_map.insert(
        "out-archiveext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_archive_extension(s.to_string()))
        }),
    );
    arg_map.insert(
        "out-archivefilepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_archive_file_pattern(s.to_string()))
        }),
    );
    arg_map.insert(
        "out-dirpattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_dir_pattern(s.to_string()))
        }),
    );
    arg_map.insert(
        "out-ext".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_extension(s.to_string()))
        }),
    );
    arg_map.insert(
        "out-filepattern".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.add_out_file_pattern(s.to_string()))
        }),
    );
    arg_map.insert(
        "out-filetype".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            let filetype = FileTypes::file_type_for_name(&s.to_string());
            Ok(settings.add_out_file_type(filetype))
        }),
    );
    arg_map.insert(
        "path".to_string(),
        Box::new(|s: &str, settings: &mut FindSettings| {
            Ok(settings.paths.push(s.to_string()))
        }),
    );
    arg_map
}

fn get_flag_map() -> HashMap<String, FlagAction> {
    let mut flag_map: HashMap<String, FlagAction> = HashMap::with_capacity(19);
    flag_map.insert(
        "archivesonly".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_archives_only(b))),
    );
    flag_map.insert(
        "debug".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.set_debug(b))),
    );
    flag_map.insert(
        "excludearchives".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.include_archives = !b)),
    );
    flag_map.insert(
        "excludehidden".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.exclude_hidden = b)),
    );
    flag_map.insert(
        "help".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.print_usage = b)),
    );
    flag_map.insert(
        "includearchives".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.include_archives = b)),
    );
    flag_map.insert(
        "includehidden".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.exclude_hidden = !b)),
    );
    flag_map.insert(
        "listdirs".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.list_dirs = b)),
    );
    flag_map.insert(
        "listfiles".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.list_files = b)),
    );
    flag_map.insert(
        "norecursive".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.recursive = !b)),
    );
    flag_map.insert(
        "recursive".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.recursive = b)),
    );
    flag_map.insert(
        "verbose".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.verbose = b)),
    );
    flag_map.insert(
        "version".to_string(),
        Box::new(|b: bool, settings: &mut FindSettings| Ok(settings.print_version = b)),
    );
    flag_map
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
        assert!(!options.findoptions.is_empty());

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
        assert_eq!(settings.include_archives, false);
        assert_eq!(settings.list_dirs, false);
        assert_eq!(settings.list_files, true);

        assert_eq!(settings.in_extensions.len(), 2);
        assert_eq!(settings.in_extensions[0], String::from("php"));
        assert_eq!(settings.in_extensions[1], String::from("rs"));
        assert_eq!(settings.out_dir_patterns.len(), 1);
        assert_eq!(
            settings.out_dir_patterns[0].to_string(),
            String::from("debug")
        );
        assert_eq!(settings.in_file_patterns.len(), 1);
        assert_eq!(
            settings.in_file_patterns[0].to_string(),
            String::from("find")
        );
        assert_eq!(settings.in_file_types.len(), 1);
        assert_eq!(settings.in_file_types[0], FileType::Code);
        assert!(settings.debug);
        assert!(settings.verbose);
        assert_eq!(settings.paths.len(), 1);
        assert_eq!(settings.paths[0], String::from("."));
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
        assert!(!options.findoptions.is_empty());

        let json = r#"
            {
              "debug": true,
              "in-ext": ["js","ts"],
              "includehidden": true,
              "out-dirpattern": "node_module",
              "out-filepattern": ["temp"],
              "path": "~/src/xfind/"
            }"#;

        match options.settings_from_json(&json.to_string()) {
            Ok(settings) => {
                assert!(settings.debug);
                assert_eq!(settings.in_extensions.len(), 2);
                assert_eq!(settings.in_extensions[0], String::from("js"));
                assert_eq!(settings.in_extensions[1], String::from("ts"));
                assert!(!settings.exclude_hidden);
                assert_eq!(settings.out_dir_patterns.len(), 1);
                assert_eq!(
                    settings.out_dir_patterns[0].to_string(),
                    String::from("node_module")
                );
                assert_eq!(settings.out_file_patterns.len(), 1);
                assert_eq!(
                    settings.out_file_patterns[0].to_string(),
                    String::from("temp")
                );
                assert_eq!(settings.paths.len(), 1);
                assert_eq!(settings.paths[0], String::from("~/src/xfind/"));
                assert!(settings.verbose);
            },
            Err(error) => {
                log(&error.to_string());
                assert!(false)
            }
        }
    }

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
        assert!(!options.findoptions.is_empty());

        // let config = Config::from_json_file(CONFIG_FILE_PATH.to_string());
        let config = Config::new();
        let path = Path::new(config.shared_path.as_str()).join("settings.json");
        let settings_file = path.to_str().unwrap();

        let args: Vec<&str> = vec!["rsfind", "--settings-file", &settings_file];
        let args: Vec<String> = args.into_iter().map(|a| a.to_string()).collect();
        match options.settings_from_args(args.iter()) {
            Ok(settings) => {
                assert!(settings.debug);
                assert!(settings.exclude_hidden);
                assert_eq!(settings.in_extensions.len(), 2);
                assert_eq!(settings.in_extensions[0], String::from("js"));
                assert_eq!(settings.in_extensions[1], String::from("ts"));
                assert!(settings.list_dirs);
                assert!(settings.list_files);
                assert!(settings.out_dir_patterns.len() > 7);
                assert_eq!(settings.out_dir_patterns[0].to_string(), String::from("_"));
                assert_eq!(settings.paths.len(), 1);
                assert_eq!(
                    settings.paths[0],
                    String::from("~/src/xfind/")
                );
                assert!(settings.verbose);
            },
            Err(error) => {
                log(&error.to_string());
                assert!(false)
            }
        }
    }
}
