use std::env;
use std::fs;

use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub struct Config {
    pub xfind_path: String,
    pub shared_path: String,
    pub file_types_path: String,
    pub find_options_path: String,
    pub version: String,
}

#[derive(Serialize, Deserialize)]
pub struct JsonConfig {
    xfindpath: String,
    version: String,
}

pub const VERSION: &str = "1.0.0";

impl Config {
    pub fn new() -> Config {
        let xfind_path: String = match env::var("XFIND_PATH") {
            Ok(path) => path,
            Err(_error) => env::var("HOME").unwrap() + "/src/xfind",
        };
        let version = String::from(VERSION);
        Config::for_values(xfind_path, version)
    }

    pub fn for_values(xfind_path: String, version: String) -> Config {
        let shared_path = xfind_path.clone() + "/shared";
        Config {
            xfind_path: xfind_path.clone(),
            shared_path: shared_path.clone(),
            file_types_path: shared_path.clone() + "/filetypes.json",
            find_options_path: shared_path.clone() + "/findoptions.json",
            version,
        }
    }

    pub fn from_json_file(json_file_path: String) -> Config {
        let contents = fs::read_to_string(json_file_path)
            .expect("Something went wrong reading the config file");
        let json_config: JsonConfig = serde_json::from_str(&contents).unwrap();
        Config::for_values(json_config.xfindpath, json_config.version)
    }
}
