use std::collections::HashSet;
use std::fs;
use std::io;
use std::time::SystemTime;

use regex::Regex;
use walkdir::WalkDir;

use crate::filetypes::{FileType, FileTypes};
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
use crate::fileresult::FileResult;
use crate::findsettings::FindSettings;
use crate::sortby::SortBy;

pub struct Finder {
    pub file_types: FileTypes,
    pub settings: FindSettings,
}

impl Finder {
    /// Create a new Finder instance for the given settings, if valid
    pub fn new(settings: FindSettings) -> Result<Finder, FindError> {
        let file_types = match FileTypes::new() {
            Ok(file_types) => file_types,
            Err(error) => return Err(error),
        };

        if let Err(error) = Finder::validate_settings(&settings) {
            return Err(error);
        }

        Ok(Finder {
            file_types,
            settings,
        })
    }

    fn validate_settings(settings: &FindSettings) -> Result<(), FindError> {
        if settings.paths().len() < 1 {
            return Err(FindError::new("Startpath not defined"));
        }
        for path in settings.paths().iter() {
            if path == "" {
                return Err(FindError::new("Startpath not defined"));
            }
            let p = FileUtil::expand_path(path);
            let metadata = fs::metadata(&p);
            if metadata.is_err() {
                return match metadata.err().unwrap().kind() {
                    io::ErrorKind::NotFound => Err(FindError::new("Startpath not found")),
                    io::ErrorKind::PermissionDenied => {
                        Err(FindError::new("Startpath not readable"))
                    },
                    _ => {
                        Err(FindError::new(
                            "An unknown error occurred trying to read startpath",
                        ))
                    }
                }
            }
        }
        if settings.max_depth() > -1 && settings.max_depth() < settings.min_depth() {
            return Err(FindError::new("Invalid range for mindepth and maxdepth"));
        }
        if settings.max_last_mod() > 0 && settings.max_last_mod() < settings.min_last_mod() {
            return Err(FindError::new("Invalid range for minlastmod and maxlastmod"));
        }
        if settings.max_size() > 0 && settings.max_size() < settings.min_size() {
            return Err(FindError::new("Invalid range for minsize and maxsize"));
        }
        Ok(())
    }

    fn matches_any_string(&self, string: &str, strings: &[String]) -> bool {
        strings.iter().any(|s| s == string)
    }

    fn matches_any_pattern(&self, s: &String, patterns: &[Regex]) -> bool {
        patterns.iter().any(|p| p.is_match(s))
    }

    fn is_matching_dir(&self, dir: &String) -> bool {
        if !self.settings.include_hidden() && FileUtil::is_hidden(&dir) {
            return false;
        }
        (self.settings.in_dir_patterns().is_empty()
            || self.matches_any_pattern(&dir, &self.settings.in_dir_patterns()))
            && (self.settings.out_dir_patterns().is_empty()
                || !self.matches_any_pattern(&dir, &self.settings.out_dir_patterns()))
    }

    fn is_matching_extension(&self, ext: &str, in_extensions: &Vec<String>,
                             out_extensions: &Vec<String>) -> bool {
        (in_extensions.is_empty()
            || self.matches_any_string(ext, &in_extensions))
            && (out_extensions.is_empty()
            || !self.matches_any_string(ext, &out_extensions))
    }

    fn has_matching_archive_extension(&self, file_result: &FileResult) -> bool {
        if !self.settings.in_archive_extensions().is_empty()
            || !self.settings.out_archive_extensions().is_empty() {
            return match FileUtil::get_extension(&file_result.file_name) {
                Some(ext) => {
                    self.is_matching_extension(ext, self.settings.in_archive_extensions(),
                                               self.settings.out_archive_extensions())
                },
                None => {
                    self.settings.in_extensions().is_empty()
                },
            }
        }
        return true
    }

    fn has_matching_extension(&self, file_result: &FileResult) -> bool {
        if !self.settings.in_extensions().is_empty()
            || !self.settings.out_extensions().is_empty() {
            return match FileUtil::get_extension(&file_result.file_name) {
                Some(ext) => {
                    self.is_matching_extension(ext, self.settings.in_extensions(),
                                               self.settings.out_extensions())
                },
                None => {
                    self.settings.in_extensions().is_empty()
                },
            }
        }
        return true
    }

    fn is_matching_file_name(&self, file_name: &String, in_patterns: &Vec<Regex>,
                             out_patterns: &Vec<Regex>) -> bool {
        (in_patterns.is_empty()
            || self.matches_any_pattern(&file_name, &in_patterns))
            && (out_patterns.is_empty()
            || !self.matches_any_pattern(&file_name, &out_patterns))
    }

    fn has_matching_archive_file_name(&self, file_result: &FileResult) -> bool {
        self.is_matching_file_name(&file_result.file_name, self.settings.in_archive_file_patterns(),
                                   self.settings.out_archive_file_patterns())
    }

    fn has_matching_file_name(&self, file_result: &FileResult) -> bool {
        self.is_matching_file_name(&file_result.file_name, self.settings.in_file_patterns(),
                                   self.settings.out_file_patterns())
    }

    fn has_matching_file_type(&self, file_result: &FileResult) -> bool {
        (self.settings.in_file_types().is_empty()
            || self.settings.in_file_types().contains(&file_result.file_type))
            && (self.settings.out_file_types().is_empty()
            || !self.settings.out_file_types().contains(&file_result.file_type))
    }

    fn has_matching_file_size(&self, file_result: &FileResult) -> bool {
        (self.settings.max_size() == 0 || file_result.file_size <= self.settings.max_size())
            && (self.settings.min_size() == 0 || file_result.file_size >= self.settings.min_size())
    }

    fn has_matching_last_mod(&self, file_result: &FileResult) -> bool {
        (self.settings.max_last_mod() == 0 || file_result.last_mod <= self.settings.max_last_mod())
            && (self.settings.min_last_mod() == 0 || file_result.last_mod >= self.settings.min_last_mod())
    }

    fn is_matching_archive_file_result(&self, file_result: &FileResult) -> bool {
        if !self.is_matching_dir(&file_result.path) {
            return false;
        }
        if !self.settings.include_hidden() && FileUtil::is_hidden(&file_result.file_name) {
            return false;
        }

        self.has_matching_archive_extension(file_result) &&
            self.has_matching_archive_file_name(file_result)
    }

    fn is_matching_file_result(&self, file_result: &FileResult) -> bool {
        if !self.is_matching_dir(&file_result.path) {
            return false;
        }
        if !self.settings.include_hidden() && FileUtil::is_hidden(&file_result.file_name) {
            return false;
        }

        self.has_matching_extension(file_result) &&
            self.has_matching_file_name(file_result) &&
            self.has_matching_file_type(file_result) &&
            self.has_matching_file_size(file_result) &&
            self.has_matching_last_mod(file_result)
    }

    fn filter_file_result(&self, file_result: &FileResult) -> bool {
        if file_result.file_type == FileType::Archive {
            return self.settings.include_archives()
                && self.is_matching_archive_file_result(file_result);
        }
        !self.settings.archives_only() && self.is_matching_file_result(file_result)
    }

    fn cmp_by_path(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let pathcmp = fr1.path.cmp(&fr2.path);
        if pathcmp.is_eq() {
            return fr1.file_name.cmp(&fr2.file_name);
        }
        pathcmp
    }

    fn cmp_by_path_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let pathcmp = fr1.path.to_lowercase().cmp(&fr2.path.to_lowercase());
        if pathcmp.is_eq() {
            return fr1.file_name.to_lowercase().cmp(&fr2.file_name.to_lowercase());
        }
        pathcmp
    }

    fn get_cmp_by_path(&self) -> impl Fn(&FileResult, &FileResult) -> std::cmp::Ordering {
        return if self.settings.sort_case_insensitive() {
            Self::cmp_by_path_ci
        } else {
            Self::cmp_by_path
        }
    }

    fn cmp_by_name(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let namecmp = fr1.file_name.cmp(&fr2.file_name);
        if namecmp.is_eq() {
            return fr1.path.cmp(&fr2.path);
        }
        namecmp
    }

    fn cmp_by_name_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let namecmp = fr1.file_name.to_lowercase().cmp(&fr2.file_name.to_lowercase());
        if namecmp.is_eq() {
            return fr1.path.to_lowercase().cmp(&fr2.path.to_lowercase());
        }
        namecmp
    }

    fn get_cmp_by_name(&self) -> impl Fn(&FileResult, &FileResult) -> std::cmp::Ordering {
        return if self.settings.sort_case_insensitive() {
            Self::cmp_by_name_ci
        } else {
            Self::cmp_by_name
        }
    }

    fn cmp_by_size(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let sizecmp = fr1.file_size.cmp(&fr2.file_size);
        if sizecmp.is_eq() {
            return Self::cmp_by_path(fr1, fr2);
        }
        sizecmp
    }

    fn cmp_by_size_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let sizecmp = fr1.file_size.cmp(&fr2.file_size);
        if sizecmp.is_eq() {
            return Self::cmp_by_path_ci(fr1, fr2);
        }
        sizecmp
    }

    fn get_cmp_by_size(&self) -> impl Fn(&FileResult, &FileResult) -> std::cmp::Ordering {
        return if self.settings.sort_case_insensitive() {
            Self::cmp_by_size_ci
        } else {
            Self::cmp_by_size
        }
    }

    fn cmp_by_type(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let typecmp = fr1.file_type.cmp(&fr2.file_type);
        if typecmp.is_eq() {
            return Self::cmp_by_path(fr1, fr2);
        }
        typecmp
    }

    fn cmp_by_type_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let typecmp = fr1.file_type.cmp(&fr2.file_type);
        if typecmp.is_eq() {
            return Self::cmp_by_path_ci(fr1, fr2);
        }
        typecmp
    }

    fn get_cmp_by_type(&self) -> impl Fn(&FileResult, &FileResult) -> std::cmp::Ordering {
        return if self.settings.sort_case_insensitive() {
            Self::cmp_by_type_ci
        } else {
            Self::cmp_by_type
        }
    }

    fn cmp_by_last_mod(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let lastmodcmp = fr1.last_mod.cmp(&fr2.last_mod);
        if lastmodcmp.is_eq() {
            return Self::cmp_by_path(fr1, fr2);
        }
        lastmodcmp
    }

    fn cmp_by_last_mod_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let lastmodcmp = fr1.last_mod.cmp(&fr2.last_mod);
        if lastmodcmp.is_eq() {
            return Self::cmp_by_path_ci(fr1, fr2);
        }
        lastmodcmp
    }

    fn get_cmp_by_last_mod(&self) -> impl Fn(&FileResult, &FileResult) -> std::cmp::Ordering {
        return if self.settings.sort_case_insensitive() {
            Self::cmp_by_last_mod_ci
        } else {
            Self::cmp_by_last_mod
        }
    }

    pub fn sort_file_results(&self, file_results: &mut Vec<FileResult>) {
        match self.settings.sort_by() {
            SortBy::FileName => file_results.sort_by(self.get_cmp_by_name()),
            SortBy::FileSize => file_results.sort_by(self.get_cmp_by_size()),
            SortBy::FileType => file_results.sort_by(self.get_cmp_by_type()),
            SortBy::LastMod => file_results.sort_by(self.get_cmp_by_last_mod()),
            _ => file_results.sort_by(self.get_cmp_by_path()),
        }
        if self.settings.sort_descending() {
            file_results.reverse();
        }
    }

    /// Initiate a find session for the given settings and get the matching files
    pub fn find(&self) -> Result<Vec<FileResult>, FindError> {
        let mut file_results: Vec<FileResult> = Vec::new();
        for p in self.settings.paths().iter() {
            let ep = FileUtil::expand_path(p);
            let mut dir_walker = WalkDir::new(&ep).follow_links(false); // TODO: add followlinks to settings and use here
            if self.settings.max_depth() > -1 {
                dir_walker = dir_walker.max_depth(self.settings.max_depth() as usize)
            }
            if self.settings.min_depth() > -1 {
                dir_walker = dir_walker.min_depth(self.settings.min_depth() as usize)
            }
            for entry in dir_walker
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
                let file_type = self.file_types.get_file_type(&filename);
                // if file_type == FileType::Unknown {
                //     continue;
                // }
                let (file_size, last_mod) = match entry.metadata() {
                    Ok(metadata) => {
                        let fs = metadata.len();
                        let mt = match metadata.modified() {
                            Ok(m) => {
                                match m.duration_since(SystemTime::UNIX_EPOCH) {
                                    Ok(d) => d.as_secs(),
                                    Err(_) => 0u64
                                }
                            }
                            Err(_) => 0u64
                        };
                        (fs, mt)
                    }
                    Err(_) => (0u64, 0u64)
                };
                let file_result = FileResult::new(path, filename, file_type,
                                                  file_size, last_mod);
                if self.filter_file_result(&file_result) {
                    file_results.push(file_result)
                }
            }
        }
        self.sort_file_results(&mut file_results);
        Ok(file_results)
    }
}

/// Get the unique list of directories for matching files
pub fn get_matching_dirs(file_results: &[FileResult]) -> Vec<&String> {
    let mut dir_set: HashSet<&String> = HashSet::new();
    let mut dirs: Vec<&String> = Vec::new();
    for f in file_results.iter() {
        if !dir_set.contains(&f.path) {
            dirs.push(&f.path);
            dir_set.insert(&f.path);
        }
    }
    dirs
}

/// Get the unique list of filepaths for the matching files
pub fn get_matching_files(file_results: &[FileResult]) -> Vec<String> {
    let mut files: Vec<String> = Vec::new();
    for f in file_results.iter() {
        let file_path = f.file_path();
        files.push(file_path);
    }
    files
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::time::SystemTime;

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
        let file_name = String::from("codefile.js");
        let filesize: u64 = 1000;
        let last_mod = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(duration) => duration.as_secs(),
            Err(_error) => 0,
        };
        let fr = FileResult::new(path, file_name, FileType::Code, filesize,
                                 last_mod);
        assert!(finder.filter_file_result(&fr));

        let path = String::from(".");
        let file_name = String::from("codefile.ts");
        let fr = FileResult::new(path, file_name, FileType::Code, filesize,
                                 last_mod);
        assert!(finder.filter_file_result(&fr));

        let path = String::from("./temp/");
        let file_name = String::from("codefile.ts");
        let fr = FileResult::new(path, file_name, FileType::Code, filesize,
                                 last_mod);
        assert!(!finder.filter_file_result(&fr));

        let path = String::from("./.hidden/");
        let file_name = String::from("codefile.ts");
        let fr = FileResult::new(path, file_name, FileType::Code, filesize,
                                 last_mod);
        assert!(!finder.filter_file_result(&fr));

        let path = String::from(".");
        let file_name = String::from(".codefile.ts");
        let fr = FileResult::new(path, file_name, FileType::Code, filesize,
                                 last_mod);
        assert!(!finder.filter_file_result(&fr));

        let path = String::from(".");
        let file_name = String::from("archive.zip");
        let fr = FileResult::new(path, file_name, FileType::Archive, filesize,
                                 last_mod);
        assert!(!finder.filter_file_result(&fr));

        let mut settings = get_default_test_settings();
        settings.set_include_archives(true);
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let file_name = String::from("archive.zip");
        let fr = FileResult::new(path, file_name, FileType::Archive, filesize,
                                 last_mod);
        assert!(finder.filter_file_result(&fr));
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
        let file_name = String::from("codefile.js");
        let file_size: u64 = 1000;
        let last_mod = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(duration) => duration.as_secs(),
            Err(_error) => 0,
        };
        let file_result = FileResult::new(path, file_name, FileType::Code, file_size,
                                          last_mod);
        assert!(finder.is_matching_file_result(&file_result));

        let path = String::from(".");
        let file_name = String::from("codefile.ts");
        let file_result = FileResult::new(path, file_name, FileType::Code, file_size,
                                          last_mod);
        assert!(finder.is_matching_file_result(&file_result));

        let path = String::from("./temp/");
        let file_name = String::from("codefile.ts");
        let file_result = FileResult::new(path, file_name, FileType::Code, file_size,
                                          last_mod);
        assert!(!finder.is_matching_file_result(&file_result));

        let path = String::from("./.hidden/");
        let file_name = String::from("codefile.ts");
        let file_result = FileResult::new(path, file_name, FileType::Code, file_size,
                                          last_mod);
        assert!(!finder.is_matching_file_result(&file_result));

        let path = String::from("./");
        let file_name = String::from(".codefile.ts");
        let file_result = FileResult::new(path, file_name, FileType::Code, file_size,
                                          last_mod);
        assert!(!finder.is_matching_file_result(&file_result));

        let path = String::from(".");
        let file_name = String::from("archive.zip");
        let file_result = FileResult::new(path, file_name, FileType::Archive, file_size,
                                          last_mod);
        assert!(!finder.is_matching_file_result(&file_result));
    }

    #[test]
    fn test_is_matching_archive_file() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_archive_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let path = String::from(".");
        let file_name = String::from("archive.zip");
        let file_size: u64 = 1000;
        let last_mod: u64 = 0;
        let file_result = FileResult::new(path, file_name, FileType::Archive,
                                          file_size, last_mod);
        assert!(finder.is_matching_archive_file_result(&file_result));

        let path = String::from(".");
        let file_name = String::from(".archive.zip");
        let file_size: u64 = 1000;
        let last_mod: u64 = 0;
        let file_result = FileResult::new(path, file_name, FileType::Archive,
                                          file_size, last_mod);
        assert!(!finder.is_matching_archive_file_result(&file_result));

        let path = String::from("./temp");
        let file_name = String::from("archive.zip");
        let file_size: u64 = 1000;
        let last_mod: u64 = 0;
        let file_result = FileResult::new(path, file_name, FileType::Archive,
                                          file_size, last_mod);
        assert!(!finder.is_matching_archive_file_result(&file_result));

        let path = String::from(".");
        let file_name = String::from("temp_archive.zip");
        let file_size: u64 = 1000;
        let last_mod: u64 = 0;
        let file_result = FileResult::new(path, file_name, FileType::Archive,
                                          file_size, last_mod);
        assert!(!finder.is_matching_archive_file_result(&file_result));
    }

    #[test]
    fn test_find_code_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("~/src/xfind/rust"));
        settings.add_in_extension(String::from("go,rs"));
        let finder = Finder::new(settings).ok().unwrap();

        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
    }

    #[test]
    fn test_find_binary_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("~/src/xfind/java"));
        settings.add_in_extension(String::from("class"));
        let finder = Finder::new(settings).ok().unwrap();

        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
    }

    #[test]
    fn test_find_jar_files() {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("../../java/javafind"));
        settings.set_archives_only(true);
        settings.add_in_archive_extension(String::from("jar"));
        let finder = Finder::new(settings).ok().unwrap();

        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
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
        settings.set_include_archives(true);
        let finder = Finder::new(settings).ok().unwrap();

        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
    }
}
