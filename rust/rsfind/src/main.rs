use core::slice::Iter;
use std::env;
use std::process;

use crate::common::log;
use crate::finder::{get_result_dirs, get_result_files, get_result_lines};
use crate::finderror::FindError;
use crate::findresultformatter::FindResultFormatter;

pub mod color;
pub mod common;
pub mod config;
pub mod filetypes;
pub mod fileutil;
pub mod finder;
pub mod finderror;
pub mod findfile;
pub mod findoptions;
pub mod findresult;
pub mod findsettings;
pub mod findresultformatter;

fn print_error(error: FindError, options: &findoptions::FindOptions) {
    log(format!("\nERROR: {}", error.description).as_str());
    options.print_usage();
}

fn error_and_exit(error: FindError, options: &findoptions::FindOptions) {
    print_error(error, options);
    process::exit(1);
}

fn print_result_dirs(results: &Vec<findresult::FindResult>) {
    let dirs = get_result_dirs(results);
    log(format!("\nDirectories with matches ({}):", dirs.len()).as_str());
    for dir in dirs.iter() {
        log(format!("{}", dir).as_str());
    }
}

fn print_result_files(results: &Vec<findresult::FindResult>) {
    let files = get_result_files(results);
    log(format!("\nFiles with matches ({}):", files.len()).as_str());
    for file in files.iter() {
        log(format!("{}", file).as_str());
    }
}

fn print_result_lines(results: &Vec<findresult::FindResult>, unique: bool) {
    let lines = get_result_lines(results, unique);
    let lines_title = if unique { "Unique lines" } else { "Lines" };
    log(format!("\n{} with matches ({}):", lines_title, lines.len()).as_str());
    for line in lines.iter() {
        log(format!("{}", line).as_str());
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
                Ok(results) => {
                    if finder.settings.print_results {
                        let formatter = FindResultFormatter::new(
                            finder.settings.colorize, finder.settings.max_line_length);
                        log(format!("\nFind results ({}):", results.len()).as_str());
                        for r in results.iter() {
                            log(formatter.format(r).as_str());
                        }
                    }
                    if finder.settings.list_dirs {
                        print_result_dirs(&results);
                    }
                    if finder.settings.list_files {
                        print_result_files(&results);
                    }
                    if finder.settings.list_lines {
                        print_result_lines(&results, finder.settings.unique_lines);
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
            "rsfind", "-x", "rs", "-s", "find", "-D", "debug", "-f", "find", "--debug",
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
            "rsfind", "-x", "rlib", "-s", "find", "-f", "find", "--debug", startpath,
        ]
        .into_iter()
        .map(|a| a.to_string())
        .collect();

        find(args.iter());
    }
}
