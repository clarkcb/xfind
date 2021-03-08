use std::fs;
use std::io;

use regex::Regex;
use walkdir::WalkDir;

use crate::filetypes::{FileType, FileTypes};
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
use crate::findfile::FindFile;
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
        for p in settings.paths.iter() {
            if p == "" {
                return Err(FindError::new("Startpath not defined"));
            }
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

    fn is_find_dir(&self, dir: &String) -> bool {
        if self.settings.exclude_hidden && FileUtil::is_hidden(&dir) {
            return false;
        }
        (self.settings.in_dir_patterns.is_empty()
            || self.matches_any_pattern(&dir, &self.settings.in_dir_patterns))
            && (self.settings.out_dir_patterns.is_empty()
                || !self.matches_any_pattern(&dir, &self.settings.out_dir_patterns))
    }

    fn is_archive_find_file(&self, findfile: &FindFile) -> bool {
        if !self.is_find_dir(&findfile.path) {
            return false;
        }
        if FileUtil::is_hidden(&findfile.name) && self.settings.exclude_hidden {
            return false;
        }
        (self.settings.in_archive_file_patterns.is_empty()
            || self.matches_any_pattern(&findfile.name, &self.settings.in_archive_file_patterns))
            && (self.settings.out_archive_file_patterns.is_empty()
                || !self.matches_any_pattern(
                    &findfile.name,
                    &self.settings.out_archive_file_patterns,
                ))
    }

    fn is_find_file(&self, findfile: &FindFile) -> bool {
        if !self.is_find_dir(&findfile.path) {
            return false;
        }
        if self.settings.exclude_hidden && FileUtil::is_hidden(&findfile.name) {
            return false;
        }

        match FileUtil::get_extension(&findfile.name) {
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
            || self.matches_any_pattern(&findfile.name, &self.settings.in_file_patterns))
            && (self.settings.in_file_types.is_empty()
                || self.settings.in_file_types.contains(&findfile.filetype))
            && (self.settings.out_file_patterns.is_empty()
                || !self.matches_any_pattern(&findfile.name, &self.settings.out_file_patterns))
            && (self.settings.out_file_types.is_empty()
                || !self.settings.out_file_types.contains(&findfile.filetype))
    }

    fn filter_file(&self, findfile: &FindFile) -> bool {
        if findfile.filetype == FileType::Archive {
            return self.settings.include_archives && self.is_archive_find_file(findfile);
        }
        !self.settings.archives_only && self.is_find_file(findfile)
    }

    /// Initiate a find session for the given settings and get the matching files
    pub fn find(&self) -> Result<Vec<FindFile>, FindError> {
        let mut findfiles: Vec<FindFile> = Vec::new();
        for p in self.settings.paths.iter() {
            for entry in WalkDir::new(&p)
                .into_iter()
                .filter_map(|e| e.ok())
                .filter(|e| e.file_type().is_file())
            {
                let path = match entry.path().parent() {
                    Some(parent) => parent.to_str().unwrap().to_string(),
                    None => ".".to_string(),
                };
                if !self.is_find_dir(&path) {
                    continue;
                }
                let filename = entry.file_name().to_str().unwrap().to_string();
                let filetype = self.filetypes.get_file_type(&filename);
                // if filetype == FileType::Unknown {
                //     continue;
                // }
                let findfile = FindFile::new(path, filename, filetype);
                if self.filter_file(&findfile) {
                    findfiles.push(findfile)
                }
            }
        }
        Ok(findfiles)
    }
}

/// Get the unique list of directories for matching files
pub fn get_matching_dirs(findfiles: &[FindFile]) -> Vec<&String> {
    let mut dirs: Vec<&String> = Vec::new();
    for f in findfiles.iter() {
        dirs.push(&f.path);
    }
    dirs.sort_unstable();
    dirs.dedup();
    dirs
}

/// Get the unique list of filepaths for the matching files
pub fn get_matching_files(findfiles: &[FindFile]) -> Vec<String> {
    let mut files: Vec<String> = Vec::new();
    for f in findfiles.iter() {
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
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.filter_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from(".codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.filter_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.filter_file(&file));

        let mut settings = get_default_test_settings();
        settings.include_archives = true;
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(finder.filter_file(&file));
    }

    #[test]
    fn test_is_find_dir() {
        let mut settings = get_default_test_settings();
        settings.add_out_dir_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        assert!(finder.is_find_dir(&path));

        let path = String::from(".git");
        assert!(!finder.is_find_dir(&path));

        let path = String::from("./temp/");
        assert!(!finder.is_find_dir(&path));
    }

    #[test]
    fn test_is_find_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("codefile.js");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.is_find_file(&file));

        let path = String::from(".");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(finder.is_find_file(&file));

        let path = String::from("./temp/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.is_find_file(&file));

        let path = String::from("./.hidden/");
        let filename = String::from("codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.is_find_file(&file));

        let path = String::from("./");
        let filename = String::from(".codefile.ts");
        let file = FindFile::new(path, filename, FileType::Code);
        assert!(!finder.is_find_file(&file));

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_find_file(&file));
    }

    #[test]
    fn test_is_archive_find_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_archive_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(finder.is_archive_find_file(&file));

        let path = String::from(".");
        let filename = String::from(".archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_archive_find_file(&file));

        let path = String::from("./temp");
        let filename = String::from("archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_archive_find_file(&file));

        let path = String::from(".");
        let filename = String::from("temp_archive.zip");
        let file = FindFile::new(path, filename, FileType::Archive);
        assert!(!finder.is_archive_find_file(&file));
    }

    #[test]
    fn test_find_code_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("~/src/xfind/rust"));
        settings.add_in_extension(String::from("go,rs"));
        let finder = Finder::new(settings).ok().unwrap();

        let findfiles = finder.find();
        assert!(findfiles.is_ok());
        let findfiles = findfiles.ok().unwrap();
        println!("findfiles: {}", findfiles.len());
    }

    #[test]
    fn test_find_binary_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("~/src/xfind/java"));
        settings.add_in_extension(String::from("class"));
        let finder = Finder::new(settings).ok().unwrap();

        let findfiles = finder.find();
        assert!(findfiles.is_ok());
        let findfiles = findfiles.ok().unwrap();
        println!("findfiles: {}", findfiles.len());
    }

    #[test]
    fn test_find_jar_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("../../java/javafind"));
        settings.set_archives_only(true);
        settings.add_in_archive_extension(String::from("jar"));
        let finder = Finder::new(settings).ok().unwrap();

        let findfiles = finder.find();
        assert!(findfiles.is_ok());
        let findfiles = findfiles.ok().unwrap();
        println!("findfiles: {}", findfiles.len());
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

        let findfiles = finder.find();
        assert!(findfiles.is_ok());
        let findfiles = findfiles.ok().unwrap();
        println!("findfiles: {}", findfiles.len());
    }
}
