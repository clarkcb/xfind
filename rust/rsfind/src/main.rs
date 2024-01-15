use core::slice::Iter;
use std::env;
use std::process;

use crate::common::{log, log_err};
use crate::finder::{get_matching_dirs, get_matching_files};
use crate::finderror::FindError;

pub mod common;
pub mod config;
pub mod filetypes;
pub mod fileutil;
pub mod finder;
pub mod finderror;
pub mod fileresult;
pub mod findoptions;
pub mod findsettings;
pub mod sortby;

fn print_error(error: FindError, options: &findoptions::FindOptions) {
    log("");
    log_err(error.description.as_str());
    options.print_usage();
}

fn error_and_exit(error: FindError, options: &findoptions::FindOptions) {
    print_error(error, options);
    process::exit(1);
}

fn print_matching_dirs(file_results: &Vec<fileresult::FileResult>) {
    let dirs = get_matching_dirs(file_results);
    if dirs.is_empty() {
        log("\nMatching directories: 0");
    } else {
        log(format!("\nMatching directories ({}):", dirs.len()).as_str());
        for dir in dirs.iter() {
            log(format!("{}", dir).as_str());
        }
    }
}

fn print_matching_files(file_results: &Vec<fileresult::FileResult>) {
    let files = get_matching_files(file_results);
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
            log_err(error.description.as_str());
            process::exit(1);
        }
    };

    match options.settings_from_args(args) {
        Ok(settings) => {
            if settings.debug() {
                log(format!("settings: {:?}", settings).as_str());
            }
            if settings.print_usage() {
                options.print_usage();
                process::exit(0);
            }
            if settings.print_version() {
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
                Ok(file_results) => {
                    if finder.settings.print_dirs() {
                        print_matching_dirs(&file_results);
                    }
                    if finder.settings.print_files() {
                        print_matching_files(&file_results);
                    }
                }
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
        let start_path = "/Users/cary/src/xfind/rust";

        let args: Vec<String> = vec![
            "rsfind", "-x", "rs", "-D", "debug", "-f", "find", "--debug",
            start_path,
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();

        find(args.iter());
    }

    #[test]
    fn test_find_binary_files() {
        let start_path = "/Users/cary/src/xfind/rust";

        let args: Vec<String> = vec![
            "rsfind", "-x", "rlib", "-f", "find", "--debug", start_path,
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();

        find(args.iter());
    }
}
