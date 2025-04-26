use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use regex::Regex;

use crate::fileresult::FileResult;
use crate::filetypes::{FileType, FileTypes};
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
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
            let p = FileUtil::expand_path_string(path);
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

    fn matches_any_pattern(&self, s: &str, patterns: &[Regex]) -> bool {
        patterns.iter().any(|p| p.is_match(s))
    }

    fn is_matching_dir(&self, dir: &str) -> bool {
        // TODO: verify check for hidden elsewhere
        if !self.settings.include_hidden() && FileUtil::is_hidden(&dir) {
            return false;
        }
        (self.settings.in_dir_patterns().is_empty()
            || self.matches_any_pattern(&dir, &self.settings.in_dir_patterns()))
            && (self.settings.out_dir_patterns().is_empty()
                || !self.matches_any_pattern(&dir, &self.settings.out_dir_patterns()))
    }

    fn is_matching_archive_extension(&self, ext: &str) -> bool {
        (self.settings.in_archive_extensions().is_empty()
            || self.matches_any_string(ext, &self.settings.in_archive_extensions()))
            && (self.settings.out_archive_extensions().is_empty()
            || !self.matches_any_string(ext, &self.settings.out_archive_extensions()))
    }

    fn is_matching_extension(&self, ext: &str) -> bool {
        (self.settings.in_extensions().is_empty()
            || self.matches_any_string(ext, &self.settings.in_extensions()))
            && (self.settings.out_extensions().is_empty()
            || !self.matches_any_string(ext, &self.settings.out_extensions()))
    }

    fn is_matching_archive_file_name(&self, file_name: &str) -> bool {
        (self.settings.in_archive_file_patterns().is_empty()
            || self.matches_any_pattern(&file_name, &self.settings.in_archive_file_patterns()))
            && (self.settings.out_archive_file_patterns().is_empty()
            || !self.matches_any_pattern(&file_name, &self.settings.out_archive_file_patterns()))
    }

    fn is_matching_file_name(&self, file_name: &str) -> bool {
        // TODO: verify check for hidden elsewhere
        if !self.settings.include_hidden() && FileUtil::is_hidden(&file_name) {
            return false;
        }
        (self.settings.in_file_patterns().is_empty()
            || self.matches_any_pattern(&file_name, &self.settings.in_file_patterns()))
            && (self.settings.out_file_patterns().is_empty()
            || !self.matches_any_pattern(&file_name, &self.settings.out_file_patterns()))
    }

    fn is_matching_file_type(&self, file_type: &FileType) -> bool {
        (self.settings.in_file_types().is_empty()
            || self.settings.in_file_types().contains(&file_type))
            && (self.settings.out_file_types().is_empty()
            || !self.settings.out_file_types().contains(&file_type))
    }

    fn is_matching_file_size(&self, file_size: &u64) -> bool {
        (self.settings.max_size() == 0 || file_size <= &self.settings.max_size())
            && (self.settings.min_size() == 0 || file_size >= &self.settings.min_size())
    }

    fn is_matching_last_mod(&self, last_mod: &u64) -> bool {
        (self.settings.max_last_mod() == 0 || last_mod <= &self.settings.max_last_mod())
            && (self.settings.min_last_mod() == 0 || last_mod >= &self.settings.min_last_mod())
    }

    fn filter_paths_to_file_results(&self, file_paths: &Vec<PathBuf>) -> Vec<FileResult> {
        file_paths.iter()
            .map(|p| self.filter_path_to_file_result(p))
            .filter(|p| p.is_some())
            .map(|p| p.unwrap()).collect()
    }

    fn filter_path_to_file_result(&self, file_path: &Path) -> Option<FileResult> {
        if !self.settings.include_hidden() && FileUtil::is_hidden_path(file_path) {
            return None;
        }
        let file_type = self.file_types.get_file_type_for_path(&file_path);
        let (file_size, last_mod) =
            if self.settings.need_last_mod() || self.settings.need_size() {
                match file_path.metadata() {
                    Ok(metadata) => {
                        let fs = if self.settings.need_size() { metadata.len() } else { 0u64 };
                        let lm = if self.settings.need_last_mod() {
                            match metadata.modified() {
                                Ok(m) => {
                                    match m.duration_since(SystemTime::UNIX_EPOCH) {
                                        Ok(d) => d.as_secs(),
                                        Err(_) => 0u64
                                    }
                                }
                                Err(_) => 0u64
                            }
                        } else {
                            0u64
                        };
                        (fs, lm)
                    }
                    Err(_) => (0u64, 0u64)
                }
            } else {
                (0u64, 0u64)
            };
        self.filter_to_file_result(file_path, &file_type, file_size, last_mod)
    }

    fn filter_to_file_result(&self, file_path: &Path, file_type: &FileType, file_size: u64, last_mod: u64) -> Option<FileResult> {
        if !self.settings.include_hidden() && FileUtil::is_hidden_path(file_path) {
            return None;
        }
        let parent = match file_path.parent() {
            Some(p) => String::from(p.to_str()?),
            None => String::from(".")
        };
        if !self.is_matching_dir(&parent) {
            return None;
        }
        let extension = match file_path.extension() {
            Some(ext) => ext.to_str()?,
            None => ""
        };
        let file_name = file_path.file_name().unwrap().to_str()?;
        if file_type == &FileType::Archive {
            if !self.settings.include_archives() && !self.settings.archives_only() {
                return None;
            }
            if !self.is_matching_archive_extension(&extension)
                || !self.is_matching_archive_file_name(&file_name) {
                return None;
            }
        } else {
            if !self.is_matching_extension(&extension)
                || !self.is_matching_file_name(&file_name)
                || !self.is_matching_file_type(&file_type) {
                return None;
            }
        }
        if !self.is_matching_file_size(&file_size) || !self.is_matching_last_mod(&last_mod) {
            return None;
        }
        let file_result = FileResult::with_path(file_path, file_type.clone(),
                                          file_size, last_mod);
        Some(file_result)
    }

    fn cmp_by_path(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let path_cmp = fr1.parent().cmp(&fr2.parent());
        if path_cmp.is_eq() {
            return fr1.file_name().cmp(&fr2.file_name());
        }
        path_cmp
    }

    fn cmp_by_path_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let path_cmp = fr1.parent().to_lowercase().cmp(&fr2.parent().to_lowercase());
        if path_cmp.is_eq() {
            return fr1.file_name().to_lowercase().cmp(&fr2.file_name().to_lowercase());
        }
        path_cmp
    }

    fn cmp_by_name(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let name_cmp = fr1.file_name().cmp(&fr2.file_name());
        if name_cmp.is_eq() {
            return fr1.parent().cmp(&fr2.parent());
        }
        name_cmp
    }

    fn cmp_by_name_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let name_cmp = fr1.file_name().to_lowercase().cmp(&fr2.file_name().to_lowercase());
        if name_cmp.is_eq() {
            return fr1.parent().to_lowercase().cmp(&fr2.parent().to_lowercase());
        }
        name_cmp
    }

    fn cmp_by_size(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let size_cmp = fr1.file_size.cmp(&fr2.file_size);
        if size_cmp.is_eq() {
            return Self::cmp_by_path(fr1, fr2);
        }
        size_cmp
    }

    fn cmp_by_size_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let size_cmp = fr1.file_size.cmp(&fr2.file_size);
        if size_cmp.is_eq() {
            return Self::cmp_by_path_ci(fr1, fr2);
        }
        size_cmp
    }

    fn cmp_by_type(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let type_cmp = fr1.file_type.cmp(&fr2.file_type);
        if type_cmp.is_eq() {
            return Self::cmp_by_path(fr1, fr2);
        }
        type_cmp
    }

    fn cmp_by_type_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let type_cmp = fr1.file_type.cmp(&fr2.file_type);
        if type_cmp.is_eq() {
            return Self::cmp_by_path_ci(fr1, fr2);
        }
        type_cmp
    }

    fn cmp_by_last_mod(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let last_mod_cmp = fr1.last_mod.cmp(&fr2.last_mod);
        if last_mod_cmp.is_eq() {
            return Self::cmp_by_path(fr1, fr2);
        }
        last_mod_cmp
    }

    fn cmp_by_last_mod_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
        let last_mod_cmp = fr1.last_mod.cmp(&fr2.last_mod);
        if last_mod_cmp.is_eq() {
            return Self::cmp_by_path_ci(fr1, fr2);
        }
        last_mod_cmp
    }

    pub fn get_sort_comparator(&self) -> impl Fn(&FileResult, &FileResult) -> std::cmp::Ordering + use<> {
        match (self.settings.sort_by(), self.settings.sort_case_insensitive(), self.settings.sort_descending()) {
            (SortBy::FileName, false, false) => Self::cmp_by_name,
            (SortBy::FileName, false, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_name(fr2, fr1),
            (SortBy::FileName, true, false) => Self::cmp_by_name_ci,
            (SortBy::FileName, true, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_name_ci(fr2, fr1),
            (SortBy::FilePath, false, false) => Self::cmp_by_path,
            (SortBy::FilePath, false, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_path(fr2, fr1),
            (SortBy::FilePath, true, false) => Self::cmp_by_path_ci,
            (SortBy::FilePath, true, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_path_ci(fr2, fr1),
            (SortBy::FileSize, false, false) => Self::cmp_by_size,
            (SortBy::FileSize, false, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_size(fr2, fr1),
            (SortBy::FileSize, true, false) => Self::cmp_by_size_ci,
            (SortBy::FileSize, true, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_size_ci(fr2, fr1),
            (SortBy::FileType, false, false) => Self::cmp_by_type,
            (SortBy::FileType, false, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_type(fr2, fr1),
            (SortBy::FileType, true, false) => Self::cmp_by_type_ci,
            (SortBy::FileType, true, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_type_ci(fr2, fr1),
            (SortBy::LastMod, false, false) => Self::cmp_by_last_mod,
            (SortBy::LastMod, false, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_last_mod(fr2, fr1),
            (SortBy::LastMod, true, false) => Self::cmp_by_last_mod_ci,
            (SortBy::LastMod, true, true) => |fr1: &FileResult, fr2: &FileResult| Self::cmp_by_last_mod_ci(fr2, fr1),
        }
    }

    pub fn sort_file_results(&self, file_results: &mut Vec<FileResult>) {
        let sort_comparator = self.get_sort_comparator();
        file_results.sort_by(sort_comparator);
    }

    fn rec_find_path(&self, dir_path: &PathBuf, min_depth: i32, max_depth: i32, current_depth: i32) -> Result<Vec<FileResult>, FindError> {
        let mut file_results: Vec<FileResult> = Vec::new();
        if max_depth > -1 && current_depth > max_depth {
            return Ok(file_results);
        }
        let recurse = !(current_depth == max_depth);

        let mut path_dirs = Vec::<PathBuf>::new();
        let mut path_files = Vec::<PathBuf>::new();

        for dir_entry in dir_path.read_dir().ok().unwrap().flatten() {
            if dir_entry.path().is_symlink() && !self.settings.follow_symlinks() {
                continue;
            }
            if dir_entry.path().is_dir() && recurse && self.is_matching_dir(dir_entry.file_name().to_str().unwrap()) {
                path_dirs.push(dir_entry.path());
            } else if dir_entry.path().is_file() && (min_depth < 0 || current_depth >= min_depth) {
                path_files.push(dir_entry.path());
            }
        }

        if !path_files.is_empty() {
            let path_file_results = self.filter_paths_to_file_results(&path_files);
            file_results.extend(path_file_results);
        }

        for path_dir in path_dirs {
            match self.rec_find_path(&path_dir, min_depth, max_depth, current_depth + 1) {
                Ok(path_dir_results) => file_results.extend(path_dir_results),
                Err(err) => return Err(err)
            }
        }

        Ok(file_results)
    }

    pub fn find_path(&self, path: &String) -> Result<Vec<FileResult>, FindError> {
        let mut file_results: Vec<FileResult> = Vec::new();
        let mut path_buf = PathBuf::from(&path);
        if !path_buf.exists() {
            path_buf = FileUtil::expand_path(&path_buf);
        }
        if path_buf.is_dir() {
            if self.settings.max_depth() == 0 {
                return Ok(file_results);
            }
            if self.is_matching_dir(path) {
                let max_depth = if self.settings.recursive() { self.settings.max_depth() } else { 1 };
                return self.rec_find_path(&path_buf, self.settings.min_depth(), max_depth, 1);
            }
        } else {
            if self.settings.min_depth() > 0 {
                return Ok(file_results);
            }
            match self.filter_path_to_file_result(path_buf.as_path()) {
                Some(file_result) => file_results.push(file_result),
                None => {}
            }
        }
        Ok(file_results)
    }

    /// Initiate a find session for the given settings and get the matching files
    pub fn find(&self) -> Result<Vec<FileResult>, FindError> {
        let mut file_results: Vec<FileResult> = Vec::new();
        for p in self.settings.paths().iter() {
            file_results.extend(self.find_path(p).unwrap_or(Vec::new()));
        }
        self.sort_file_results(&mut file_results);
        Ok(file_results)
    }
}

/// Get the unique list of directories for matching files
pub fn get_matching_dirs(file_results: &[FileResult]) -> Vec<String> {
    let mut dir_set: HashSet<String> = HashSet::new();
    let mut dirs: Vec<String> = Vec::new();
    for f in file_results.iter() {
        if !dir_set.contains(f.parent()) {
            dirs.push(String::from(f.parent()));
            dir_set.insert(String::from(f.parent()));
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

    use crate::config::Config;
    use crate::filetypes::FileType;
    use super::*;

    fn get_default_test_settings() -> FindSettings {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("."));
        settings
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
    fn test_is_matching_file_name() {
        let mut settings = get_default_test_settings();
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        let file_name = String::from("finder.rs");
        assert!(finder.is_matching_file_name(&file_name));

        let file_name = String::from(".gitignore");
        assert!(!finder.is_matching_file_name(&file_name));

        let file_name = String::from("tempfile.rs");
        assert!(!finder.is_matching_file_name(&file_name));
    }

    #[test]
    fn test_is_matching_file_size() {
        let mut settings = get_default_test_settings();
        settings.set_min_size(1000u64);
        settings.set_max_size(5000u64);
        let finder = Finder::new(settings).ok().unwrap();

        // in between
        let file_size = 3000u64;
        assert!(finder.is_matching_file_size(&file_size));

        // matches min size
        let file_size = 1000u64;
        assert!(finder.is_matching_file_size(&file_size));

        // below min size
        let file_size = 100u64;
        assert!(!finder.is_matching_file_size(&file_size));

        // matches max size
        let file_size = 5000u64;
        assert!(finder.is_matching_file_size(&file_size));

        // below min size
        let file_size = 5500u64;
        assert!(!finder.is_matching_file_size(&file_size));
    }

    #[test]
    fn test_filter_to_file_result() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        // js extension
        let file_path = Path::new("./codefile.js");
        let file_type = FileType::Code;
        let file_size: u64 = 1000;
        let last_mod = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(duration) => duration.as_secs(),
            Err(_error) => 0,
        };
        assert!(finder.filter_to_file_result(&file_path, &file_type, file_size, last_mod).is_some());

        // ts extension
        let file_path = Path::new("./codefile.ts");
        assert!(finder.filter_to_file_result(&file_path, &file_type, file_size, last_mod).is_some());

        // "temp" in path
        let file_path = Path::new("./temp/codefile.ts");
        assert!(finder.filter_to_file_result(&file_path, &file_type, file_size, last_mod).is_none());

        // hidden path
        let file_path = Path::new("./.hidden/codefile.ts");
        assert!(finder.filter_to_file_result(&file_path, &file_type, file_size, last_mod).is_none());

        // hidden file name
        let file_path = Path::new("./.codefile.ts");
        assert!(finder.filter_to_file_result(&file_path, &file_type, file_size, last_mod).is_none());

        // archive file
        let file_path = Path::new("./archive.zip");
        let file_type = FileType::Archive;
        assert!(finder.filter_to_file_result(&file_path, &file_type, file_size, last_mod).is_none());

        // archive file + include_archives
        let mut settings = get_default_test_settings();
        settings.set_include_archives(true);
        let finder = Finder::new(settings).ok().unwrap();
        assert!(finder.filter_to_file_result(&file_path, &file_type, file_size, last_mod).is_some());
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
        let path_string = if path.exists() {
            String::from("../../shared/testFiles.zip")
        } else {
            String::from("../../shared")
        };
        settings.add_path(path_string);
        settings.set_include_archives(true);
        let finder = Finder::new(settings).ok().unwrap();

        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
    }

    #[test]
    fn test_follow_symlinks_default_settings() {
        let mut settings = FindSettings::default();
        let config = Config::new();
        let bin_path = Path::new(config.xfind_path.as_str()).join("bin");
        settings.add_path(bin_path.to_str().unwrap().to_string());

        let finder = Finder::new(settings).unwrap();
        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
        assert!(file_results.len() < 3);
    }

    #[test]
    fn test_follow_symlinks_with_follow_symlinks() {
        let mut settings = FindSettings::default();
        let config = Config::new();
        let bin_path = Path::new(config.xfind_path.as_str()).join("bin");
        settings.add_path(bin_path.to_str().unwrap().to_string());
        settings.set_follow_symlinks(true);

        let finder = Finder::new(settings).unwrap();
        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
        assert!(file_results.len() == 0 || file_results.len() > 2);
    }

    #[test]
    fn test_follow_symlinks_no_follow_symlinks() {
        let mut settings = FindSettings::default();
        let config = Config::new();
        let bin_path = Path::new(config.xfind_path.as_str()).join("bin");
        settings.add_path(bin_path.to_str().unwrap().to_string());
        settings.set_follow_symlinks(false);

        let finder = Finder::new(settings).unwrap();
        let file_results = finder.find();
        assert!(file_results.is_ok());
        let file_results = file_results.ok().unwrap();
        println!("file_results: {}", file_results.len());
        assert!(file_results.len() < 3);
    }
}
