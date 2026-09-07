use std::env;
use std::fs;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
pub struct FindConfig {
    pub xfind_path: String,
    pub shared_path: String,
    pub file_types_path: String,
    pub find_options_path: String,
    pub default_find_settings_path: String,
    pub version: String,
}

#[derive(Serialize, Deserialize)]
pub struct JsonFindConfig {
    xfindconfigdir: String,
    xfindpath: String,
    version: String,
}

pub const VERSION: &str = "1.0.0";

impl FindConfig {
    pub fn new() -> FindConfig {
        let default_xfind_config_dir: String = env::var("HOME").unwrap() + "/.config/xfind";
        let xfind_config_dir: String = env::var("XFIND_CONFIG_DIR")
            .unwrap_or_else(|_error| default_xfind_config_dir);
        let default_xfind_path: String = env::var("HOME").unwrap() + "/src/xfind";
        let xfind_path: String = env::var("XFIND_PATH")
            .unwrap_or_else(|_error| default_xfind_path);
        let version = String::from(VERSION);
        FindConfig::for_values(xfind_config_dir, xfind_path, version)
    }

    pub fn for_values(xfind_config_dir: String, xfind_path: String, version: String) -> FindConfig {
        let shared_path = xfind_path.clone() + "/shared";
        FindConfig {
            xfind_path: xfind_path.clone(),
            shared_path: shared_path.clone(),
            file_types_path: shared_path.clone() + "/filetypes.json",
            find_options_path: shared_path.clone() + "/findoptions.json",
            default_find_settings_path: xfind_config_dir.clone() + "/settings.json",
            version,
        }
    }

    pub fn from_json_file(json_file_path: String) -> FindConfig {
        let contents = fs::read_to_string(json_file_path)
            .expect("Something went wrong reading the config file");
        let json_config: JsonFindConfig = serde_json::from_str(&contents).unwrap();
        FindConfig::for_values(json_config.xfindconfigdir, json_config.xfindpath, json_config.version)
    }
}
