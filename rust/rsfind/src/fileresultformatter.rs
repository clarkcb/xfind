use crate::consolecolor::{GREEN, RESET};
use crate::fileresult::FileResult;
use crate::findsettings::FindSettings;
use std::path::PathBuf;

pub struct FileResultFormatter {
    pub settings: FindSettings,
    pub format_dir_path: Box<dyn Fn(&PathBuf, &FindSettings) -> String>,
    pub format_file_name: Box<dyn Fn(&str, &FindSettings) -> String>,
}

pub fn colorize(s: &str, match_start_index: usize, match_end_index: usize) -> String {
    let mut prefix = String::from("");
    if match_start_index > 0 {
        prefix = String::from(&s[0..match_start_index]);
    }
    let mut suffix = String::from("");
    if match_end_index < s.len() {
        suffix = String::from(&s[match_end_index..]);
    }
    String::from(format!("{}{}{}{}{}",
                         prefix,
                         GREEN,
                         &s[match_start_index..match_end_index],
                         RESET,
                         suffix))
}

fn format_dir_path_with_color(dir_path: &PathBuf, settings: &FindSettings) -> String {
    let mut formatted_dir_path = String::from(dir_path.to_str().unwrap());
    for p in settings.in_dir_patterns() {
        let m = p.find(&formatted_dir_path);
        if m.is_some() {
            formatted_dir_path = colorize(&formatted_dir_path, m.unwrap().start(),
                                          m.unwrap().end());
            break;
        }
    }
    formatted_dir_path
}

fn format_file_name_with_color(file_name: &str, settings: &FindSettings) -> String {
    let mut formatted_file_name = String::from(file_name);
    for p in settings.in_file_patterns() {
        let m = p.find(&file_name);
        if m.is_some() {
            formatted_file_name = colorize(&formatted_file_name, m.unwrap().start(),
                                           m.unwrap().end());
            break;
        }
    }
    if !settings.in_extensions().is_empty() {
        let idx = formatted_file_name.rfind('.');
        if idx.is_some() && idx.unwrap() > 0 && idx.unwrap() < formatted_file_name.len() {
            formatted_file_name = colorize(&formatted_file_name,
                                           idx.unwrap() + 1,
                                           formatted_file_name.len());
        }
    }
    formatted_file_name
}

impl FileResultFormatter {
    pub fn new(settings: FindSettings) -> FileResultFormatter {
        let _format_dir_path: fn(&PathBuf, &FindSettings) -> String =
            if settings.colorize() && !settings.in_dir_patterns().is_empty() {
                |dir_path: &PathBuf, settings: &FindSettings| format_dir_path_with_color(&dir_path, &settings)
            } else {
                |dir_path: &PathBuf, _settings: &FindSettings| String::from(dir_path.to_str().unwrap())
            };
        let _format_file_name: fn(&str, &FindSettings) -> String =
            if settings.colorize() && (!settings.in_extensions().is_empty() || !settings.in_file_patterns().is_empty()) {
                |file_name: &str, settings: &FindSettings| format_file_name_with_color(&file_name, &settings)
            } else {
                |file_name: &str, _settings: &FindSettings| String::from(file_name)
            };

        Self {
            settings,
            format_dir_path: Box::new(_format_dir_path),
            format_file_name: Box::new(_format_file_name),
        }
    }

    pub fn format_file_path(&self, file_path: &PathBuf) -> String {
        let mut parent = String::from(".");
        if file_path.parent().is_some() {
            parent = (self.format_dir_path)(&file_path.parent().unwrap().to_path_buf(), &self.settings);
        }
        let file_name = (self.format_file_name)(&file_path.file_name().unwrap().to_str().unwrap().to_string(), &self.settings);
        PathBuf::from(parent).join(file_name).display().to_string()
    }

    pub fn format_file_result(&self, file_result: &FileResult) -> String {
        self.format_file_path(&file_result.file_path)
    }
}
