use std::fs;
use std::io;

use regex::Regex;
use walkdir::WalkDir;

use crate::filetypes::{FileType, FileTypes};
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
use crate::fileresult::FileResult;
use crate::findsettings::FindSettings;

pub struct Finder {
    pub filetypes: FileTypes,
    pub settings: FindSettings,
}

impl Finder {
    /// Create a new Finder instance for the given settings, if valid
    pub fn new(settings: FindSettings) -> Result<Finder, FindError> {
        let filetypes = match FileTypes::new() {
            Ok(filetypes) => filetypes,
            Err(error) => return Err(error),
        };

        if let Err(error) = Finder::validate_settings(&settings) {
            return Err(error);
        }

        Ok(Finder {
            filetypes,
            settings,
        })
    }

    fn validate_settings(settings: &FindSettings) -> Result<(), FindError> {
        if settings.paths.len() < 1 {
            return Err(FindError::new("Startpath not defined"));
        }
        for path in settings.paths.iter() {
            if path == "" {
                return Err(FindError::new("Startpath not defined"));
            }
            let p = FileUtil::expand_path(path);
            let metadata = fs::metadata(&p);
            if metadata.is_err() {
                match metadata.err().unwrap().kind() {
                    io::ErrorKind::NotFound => return Err(FindError::new("Startpath not found")),
                    io::ErrorKind::PermissionDenied => {
                        return Err(FindError::new("Startpath not readable"))
                    },
                    _ => {
                        return Err(FindError::new(
                            "An unknown error occurred trying to read startpath",
                        ))
                    }
                }
            }
        }
        Ok(())
    }

    fn matches_any_string(&self, string: &str, strings: &[String]) -> bool {
        strings.iter().any(|s| s == string)
    }

    fn matches_any_pattern(&self, s: &String, patterns: &[Regex]) -> bool {
        for p in patterns.iter() {
            if p.is_match(s) {
                return true;
            }
        }
        false
    }

    fn is_matching_dir(&self, dir: &String) -> bool {
        if self.settings.exclude_hidden && FileUtil::is_hidden(&dir) {
            return false;
        }
        (self.settings.in_dir_patterns.is_empty()
            || self.matches_any_pattern(&dir, &self.settings.in_dir_patterns))
            && (self.settings.out_dir_patterns.is_empty()
                || !self.matches_any_pattern(&dir, &self.settings.out_dir_patterns))
    }

    fn is_matching_archive_file(&self, fileresult: &FileResult) -> bool {
        if !self.is_matching_dir(&fileresult.path) {
            return false;
        }
        if FileUtil::is_hidden(&fileresult.name) && self.settings.exclude_hidden {
            return false;
        }
        (self.settings.in_archive_file_patterns.is_empty()
            || self.matches_any_pattern(&fileresult.name, &self.settings.in_archive_file_patterns))
            && (self.settings.out_archive_file_patterns.is_empty()
                || !self.matches_any_pattern(
                    &fileresult.name,
                    &self.settings.out_archive_file_patterns,
                ))
    }

    fn is_matching_file(&self, fileresult: &FileResult) -> bool {
        if !self.is_matching_dir(&fileresult.path) {
            return false;
        }
        if self.settings.exclude_hidden && FileUtil::is_hidden(&fileresult.name) {
            return false;
        }

        match FileUtil::get_extension(&fileresult.name) {
            Some(ext) => {
                if (!self.settings.in_extensions.is_empty()
                    && !self.matches_any_string(ext, &self.settings.in_extensions))
                    || (!self.settings.out_extensions.is_empty()
                        && self.matches_any_string(ext, &self.settings.out_extensions))
                {
                    return false;
                }
            },
            None => {
                if !self.settings.in_extensions.is_empty() {
                    return false;
                }
            },
        }

        (self.settings.in_file_patterns.is_empty()
            || self.matches_any_pattern(&fileresult.name, &self.settings.in_file_patterns))
            && (self.settings.in_file_types.is_empty()
                || self.settings.in_file_types.contains(&fileresult.filetype))
            && (self.settings.out_file_patterns.is_empty()
                || !self.matches_any_pattern(&fileresult.name, &self.settings.out_file_patterns))
            && (self.settings.out_file_types.is_empty()
                || !self.settings.out_file_types.contains(&fileresult.filetype))
    }

    fn filter_file(&self, fileresult: &FileResult) -> bool {
        if fileresult.filetype == FileType::Archive {
            return self.settings.include_archives && self.is_matching_archive_file(fileresult);
        }
        !self.settings.archives_only && self.is_matching_file(fileresult)
    }

    /// Initiate a find session for the given settings and get the matching files
    pub fn find(&self) -> Result<Vec<FileResult>, FindError> {
        let mut fileresults: Vec<FileResult> = Vec::new();
        for p in self.settings.paths.iter() {
            let ep = FileUtil::expand_path(p);
            for entry in WalkDir::new(&ep)
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| e.file_type().is_file())
            {
                let path = match entry.path().parent() {
                    Some(parent) => parent.to_str().unwrap().to_string(),
                    None => ".".to_string(),
                };
                if !self.is_matching_dir(&path) {
                    continue;
                }
                let filename = entry.file_name().to_str().unwrap().to_string();
                let filetype = self.filetypes.get_file_type(&filename);
                // if filetype == FileType::Unknown {
                //     continue;
                // }
                let fileresult = FileResult::new(path, filename, filetype);
                if self.filter_file(&fileresult) {
                    fileresults.push(fileresult)
                }
            }
        }
        Ok(fileresults)
    }
}

/// Get the unique list of directories for matching files
pub fn get_matching_dirs(fileresults: &[FileResult]) -> Vec<&String> {
    let mut dirs: Vec<&String> = Vec::new();
    for f in fileresults.iter() {
        dirs.push(&f.path);
    }
    dirs.sort_unstable();
    dirs.dedup();
    dirs
}

/// Get the unique list of filepaths for the matching files
pub fn get_matching_files(fileresults: &[FileResult]) -> Vec<String> {
    let mut files: Vec<String> = Vec::new();
    for f in fileresults.iter() {
        let filepath = f.filepath();
        files.push(filepath);
    }
    files.sort_unstable();
    files.dedup();
    files
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::filetypes::FileType;

    use super::*;

    fn get_default_test_settings() -> FindSettings {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("."));
        settings
    }

    #[test]
    fn test_filter_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("codefile.js");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(finder.filter_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from(".codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FileResult::new(path, filename, FileType::Archive);
        assert!(!finder.filter_file(&file));

        let mut settings = get_default_test_settings();
        settings.include_archives = true;
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FileResult::new(path, filename, FileType::Archive);
        assert!(finder.filter_file(&file));
    }

    #[test]
    fn test_is_matching_dir() {
        let mut settings = get_default_test_settings();
        settings.add_out_dir_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        assert!(finder.is_matching_dir(&path));

        let path = String::from(".git");
        assert!(!finder.is_matching_dir(&path));

        let path = String::from("./temp/");
        assert!(!finder.is_matching_dir(&path));
    }

    #[test]
    fn test_is_matching_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("codefile.js");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(finder.is_matching_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(finder.is_matching_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(!finder.is_matching_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(!finder.is_matching_file(&file));

        let path = String::from("./");
        let filename = String::from(".codefile.ts");
        let file = FileResult::new(path, filename, FileType::Code);
        assert!(!finder.is_matching_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FileResult::new(path, filename, FileType::Archive);
        assert!(!finder.is_matching_file(&file));
    }

    #[test]
    fn test_is_matching_archive_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_archive_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FileResult::new(path, filename, FileType::Archive);
        assert!(finder.is_matching_archive_file(&file));

        let path = String::from(".");
        let filename = String::from(".archive.zip");
        let file = FileResult::new(path, filename, FileType::Archive);
        assert!(!finder.is_matching_archive_file(&file));

        let path = String::from("./temp");
        let filename = String::from("archive.zip");
        let file = FileResult::new(path, filename, FileType::Archive);
        assert!(!finder.is_matching_archive_file(&file));

        let path = String::from(".");
        let filename = String::from("temp_archive.zip");
        let file = FileResult::new(path, filename, FileType::Archive);
        assert!(!finder.is_matching_archive_file(&file));
    }

    #[test]
    fn test_find_code_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("~/src/xfind/rust"));
        settings.add_in_extension(String::from("go,rs"));
        let finder = Finder::new(settings).ok().unwrap();

        let fileresults = finder.find();
        assert!(fileresults.is_ok());
        let fileresults = fileresults.ok().unwrap();
        println!("fileresults: {}", fileresults.len());
    }

    #[test]
    fn test_find_binary_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("~/src/xfind/java"));
        settings.add_in_extension(String::from("class"));
        let finder = Finder::new(settings).ok().unwrap();

        let fileresults = finder.find();
        assert!(fileresults.is_ok());
        let fileresults = fileresults.ok().unwrap();
        println!("fileresults: {}", fileresults.len());
    }

    #[test]
    fn test_find_jar_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("../../java/javafind"));
        settings.set_archives_only(true);
        settings.add_in_archive_extension(String::from("jar"));
        let finder = Finder::new(settings).ok().unwrap();

        let fileresults = finder.find();
        assert!(fileresults.is_ok());
        let fileresults = fileresults.ok().unwrap();
        println!("fileresults: {}", fileresults.len());
    }

    #[test]
    fn test_find_zip_file() {
        let mut settings = FindSettings::default();
        let path = Path::new("../../shared/testFiles.zip");
        let pathstring = if path.exists() {
            String::from("../../shared/testFiles.zip")
        } else {
            String::from("../../shared")
        };
        settings.add_path(pathstring);
        settings.include_archives = true;
        let finder = Finder::new(settings).ok().unwrap();

        let fileresults = finder.find();
        assert!(fileresults.is_ok());
        let fileresults = fileresults.ok().unwrap();
        println!("fileresults: {}", fileresults.len());
    }
}
