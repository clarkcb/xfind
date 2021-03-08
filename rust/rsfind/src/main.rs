use core::slice::Iter;
use std::env;
use std::process;

use crate::common::log;
use crate::finder::{get_matching_dirs, get_matching_files};
use crate::finderror::FindError;

pub mod color;
pub mod common;
pub mod config;
pub mod filetypes;
pub mod fileutil;
pub mod finder;
pub mod finderror;
pub mod findfile;
pub mod findoptions;
pub mod findsettings;

fn print_error(error: FindError, options: &findoptions::FindOptions) {
    log(format!("\nERROR: {}", error.description).as_str());
    options.print_usage();
}

fn error_and_exit(error: FindError, options: &findoptions::FindOptions) {
    print_error(error, options);
    process::exit(1);
}

fn print_matching_dirs(findfiles: &Vec<findfile::FindFile>) {
    let dirs = get_matching_dirs(findfiles);
    if dirs.is_empty() {
        log("\nMatching directories: 0");
    } else {
        log(format!("\nMatching directories ({}):", dirs.len()).as_str());
        for dir in dirs.iter() {
            log(format!("{}", dir).as_str());
        }
    }
}

fn print_matching_files(findfiles: &Vec<findfile::FindFile>) {
    let files = get_matching_files(findfiles);
    if files.is_empty() {
        log("\nMatching files: 0");
    } else {
        log(format!("\nMatching files ({}):", files.len()).as_str());
        for file in files.iter() {
            log(format!("{}", file).as_str());
        }
    }
}

fn find(args: Iter<String>) {
    let options = match findoptions::FindOptions::new() {
        Ok(options) => options,
        Err(error) => {
            log(format!("\nERROR: {}", error.description).as_str());
            process::exit(1);
        }
    };

    match options.settings_from_args(args) {
        Ok(settings) => {
            if settings.debug {
                log(format!("settings: {:?}", settings).as_str());
            }
            if settings.print_usage {
                options.print_usage();
                process::exit(0);
            }
            if settings.print_version {
                options.print_version();
                process::exit(0);
            }

            let finder = match finder::Finder::new(settings) {
                Ok(finder) => finder,
                Err(error) => {
                    print_error(error, &options);
                    process::exit(1);
                }
            };

            match finder.find() {
                Ok(findfiles) => {
                    if finder.settings.list_dirs {
                        print_matching_dirs(&findfiles);
                    }
                    if finder.settings.list_files {
                        print_matching_files(&findfiles);
                    }
                },
                Err(error) => error_and_exit(error, &options),
            }
        }
        Err(error) => {
            error_and_exit(error, &options);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    find(args.iter());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_code_files() {
        let startpath = "/Users/cary/src/xfind/rust";

        let args: Vec<String> = vec![
            "rsfind", "-x", "rs", "-D", "debug", "-f", "find", "--debug",
            startpath,
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();

        find(args.iter());
    }

    #[test]
    fn test_find_binary_files() {
        let startpath = "/Users/cary/src/xfind/rust";

        let args: Vec<String> = vec![
            "rsfind", "-x", "rlib", "-f", "find", "--debug", startpath,
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();

        find(args.iter());
    }
}
