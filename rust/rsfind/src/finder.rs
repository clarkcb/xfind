use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::common::log;
use crate::fileresult::FileResult;
use crate::fileresultformatter::FileResultFormatter;
use crate::fileresultsorter::FileResultSorter;
use crate::filetypes::{FileType, FileTypes};
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
use crate::findsettings::FindSettings;
use regex::Regex;

pub struct Finder {
    pub file_types: FileTypes,
    pub settings: FindSettings,
}

fn matches_any_pattern(s: &str, patterns: &[Regex]) -> bool {
    patterns.iter().any(|p| p.is_match(s))
}

fn matches_any_string(string: &str, strings: &[String]) -> bool {
    strings.iter().any(|s| s == string)
}

fn empty_or_matches_any_pattern(s: &str, patterns: &[Regex]) -> bool {
    patterns.is_empty() || matches_any_pattern(s, patterns)
}

fn empty_or_not_matches_any_pattern(s: &str, patterns: &[Regex]) -> bool {
    patterns.is_empty() || !matches_any_pattern(s, patterns)
}

fn empty_or_matches_any_string(string: &str, strings: &[String]) -> bool {
    strings.is_empty() || matches_any_string(string, strings)
}

fn empty_or_not_matches_any_string(string: &str, strings: &[String]) -> bool {
    strings.is_empty() || !matches_any_string(string, strings)
}

fn empty_or_matches_any_file_type(file_type: &FileType, file_types: &[FileType]) -> bool {
    file_types.is_empty() || file_types.contains(file_type)
}

fn empty_or_not_matches_any_file_type(file_type: &FileType, file_types: &[FileType]) -> bool {
    file_types.is_empty() || !file_types.contains(file_type)
}

fn is_matching_dir_path_by_hidden(settings: &FindSettings, dir_path: &Path) -> bool {
    settings.include_hidden() || !FileUtil::is_hidden_path(&dir_path)
}

fn is_matching_dir_path_by_in_patterns(settings: &FindSettings, dir_path: &Path) -> bool {
    if settings.in_dir_patterns().is_empty() {
        return true;
    }
    for elem in dir_path.iter() {
        let elem_string = elem.to_str().unwrap().to_string();
        if matches_any_pattern(&elem_string, settings.in_dir_patterns()) {
            return true;
        }
    }
    false
}

fn is_matching_dir_path_by_out_patterns(settings: &FindSettings, dir_path: &Path) -> bool {
    if settings.out_dir_patterns().is_empty() {
        return true;
    }
    for elem in dir_path.iter() {
        let elem_string = elem.to_str().unwrap().to_string();
        if matches_any_pattern(&elem_string, settings.out_dir_patterns()) {
            return false;
        }
    }
    true
}

fn is_traversable_dir_path(settings: &FindSettings, dir_path: &Path) -> bool {
    is_matching_dir_path_by_hidden(settings, dir_path)
        && is_matching_dir_path_by_out_patterns(settings, dir_path)
}

fn is_matching_dir_path(settings: &FindSettings, dir_path: &Path) -> bool {
    is_matching_dir_path_by_hidden(settings, dir_path)
        && is_matching_dir_path_by_in_patterns(settings, dir_path)
        && is_matching_dir_path_by_out_patterns(settings, dir_path)
}

fn is_matching_file_name_by_hidden(settings: &FindSettings, file_name: &str) -> bool {
    settings.include_hidden() || !FileUtil::is_hidden_name(&file_name)
}

fn is_matching_archive_extension(settings: &FindSettings, ext: &str) -> bool {
    empty_or_matches_any_string(ext, &settings.in_archive_extensions())
        && empty_or_not_matches_any_string(ext, &settings.out_archive_extensions())
}

fn is_matching_archive_file_name(settings: &FindSettings, file_name: &str) -> bool {
    empty_or_matches_any_pattern(file_name, &settings.in_archive_file_patterns())
        && empty_or_not_matches_any_pattern(file_name, &settings.out_archive_file_patterns())
}

fn is_matching_archive_file_path(settings: &FindSettings, file_path: &Path) -> bool {
    if file_path.parent().is_some() {
        if !is_matching_dir_path(settings, file_path.parent().unwrap()) {
            return false;
        }
    }
    if !settings.in_archive_extensions().is_empty() || !settings.out_archive_extensions().is_empty() {
        let ext = match file_path.extension() {
            Some(ext) => ext.to_str().unwrap(),
            None => ""
        };
        if !is_matching_archive_extension(settings, ext) {
            return false;
        }
    }
    match file_path.file_name() {
        Some(file_name) => {
            if !is_matching_archive_file_name(settings, file_name.to_str().unwrap()) {
                return false;
            }
        }
        None => return false
    }
    true
}

fn is_matching_archive_file_result(settings: &FindSettings, file_result: &FileResult) -> bool {
    if !is_matching_archive_file_path(settings, &file_result.file_path) {
        return false;
    }
    true
}

fn is_matching_extension(settings: &FindSettings, ext: &str) -> bool {
    empty_or_matches_any_string(ext, &settings.in_extensions())
        && empty_or_not_matches_any_string(ext, &settings.out_extensions())
}

fn is_matching_file_name(settings: &FindSettings, file_name: &str) -> bool {
    is_matching_file_name_by_hidden(settings, file_name)
        && empty_or_matches_any_pattern(file_name, &settings.in_file_patterns())
        && empty_or_not_matches_any_pattern(file_name, &settings.out_file_patterns())
}

fn is_matching_file_path(settings: &FindSettings, file_path: &Path) -> bool {
    if file_path.parent().is_some() {
        if !is_matching_dir_path(settings, file_path.parent().unwrap()) {
            return false;
        }
    }
    if !settings.in_extensions().is_empty() || !settings.out_extensions().is_empty() {
        let ext = match file_path.extension() {
            Some(ext) => ext.to_str().unwrap(),
            None => ""
        };
        if !is_matching_extension(settings, ext) {
            return false;
        }
    }
    match file_path.file_name() {
        Some(file_name) => {
            if !is_matching_file_name(settings, file_name.to_str().unwrap()) {
                return false;
            }
        }
        None => return false
    }
    true
}

fn is_matching_file_type(settings: &FindSettings, file_type: &FileType) -> bool {
    empty_or_matches_any_file_type(file_type, &settings.in_file_types())
        && empty_or_not_matches_any_file_type(file_type, &settings.out_file_types())
}

fn is_matching_file_size(settings: &FindSettings, file_size: &u64) -> bool {
    (settings.max_size() == 0 || file_size <= &settings.max_size())
        && (settings.min_size() == 0 || file_size >= &settings.min_size())
}

fn is_matching_last_mod(settings: &FindSettings, last_mod: &u64) -> bool {
    (settings.max_last_mod() == 0 || last_mod <= &settings.max_last_mod())
        && (settings.min_last_mod() == 0 || last_mod >= &settings.min_last_mod())
}

fn is_matching_file_result(settings: &FindSettings, file_result: &FileResult) -> bool {
    if !is_matching_file_path(settings, &file_result.file_path) {
        return false;
    }
    if !is_matching_file_type(settings, &file_result.file_type) {
        return false;
    }
    if !is_matching_file_size(settings, &file_result.file_size) {
        return false;
    }
    if !is_matching_last_mod(settings, &file_result.last_mod) {
        return false;
    }
    true
}

fn validate_settings(settings: &FindSettings) -> Result<(), FindError> {
    if settings.paths().len() < 1 {
        return Err(FindError::new("Startpath not defined"));
    }
    for path in settings.paths().iter() {
        if path == "" {
            return Err(FindError::new("Startpath not defined"));
        }
        let path_buf = PathBuf::from(&path);
        let metadata = fs::symlink_metadata(&path_buf);
        match metadata {
            Ok(data) => {
                if data.is_symlink() {
                    if !settings.follow_symlinks() {
                        return Err(FindError::new(
                            "Startpath does not match find settings",
                        ))
                    }
                } else if data.is_dir() {
                    if !is_traversable_dir_path(&settings, &path_buf) {
                        return Err(FindError::new(
                            "Startpath does not match find settings",
                        ))
                    }
                } else if data.is_file() {
                    // let path_buf = PathBuf::from(&p);
                    if !is_matching_file_path(&settings, &path_buf) {
                        return Err(FindError::new(
                            "Startpath does not match find settings",
                        ))
                    }
                } else {
                    // TODO: start path is unknown/invalid type
                    return Err(FindError::new(
                        "Startpath does not match find settings",
                    ))
                }
            }
            Err(_) => return match metadata.err().unwrap().kind() {
                io::ErrorKind::NotFound => Err(FindError::new("Startpath not found")),
                io::ErrorKind::PermissionDenied => {
                    Err(FindError::new("Startpath not readable"))
                },
                _ => {
                    Err(FindError::new(
                        "An unknown error occurred trying to read startpath",
                    ))
                }
            },
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

impl Finder {
    /// Create a new Finder instance for the given settings, if valid
    pub fn new(settings: FindSettings) -> Result<Finder, FindError> {
        let file_types = match FileTypes::new() {
            Ok(file_types) => file_types,
            Err(error) => return Err(error),
        };

        if let Err(error) = validate_settings(&settings) {
            return Err(error);
        }

        Ok(Finder {
            file_types,
            settings,
        })
    }

    fn get_file_size_and_last_mod(&self, file_path: &Path) -> (u64, u64) {
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
        }
    }

    fn filter_archive_file_path_to_file_result(&self, file_path: &Path) -> Option<FileResult> {
        if !self.settings.include_archives() && !self.settings.archives_only() {
            return None;
        }
        if !is_matching_archive_file_path(&self.settings, &file_path) {
            return None;
        }

        let file_result = FileResult::with_path(file_path, FileType::Archive, 0u64, 0u64);
        Some(file_result)
    }

    fn filter_reg_file_path_to_file_result(&self, file_path: &Path, file_type: &FileType) -> Option<FileResult> {
        if self.settings.archives_only() {
            return None;
        }

        if !is_matching_file_path(&self.settings, &file_path) {
            return None;
        }

        if !is_matching_file_type(&self.settings, &file_type) {
            return None;
        }

        let (file_size, last_mod) = self.get_file_size_and_last_mod(file_path);

        if !is_matching_file_size(&self.settings, &file_size) {
            return None;
        }

        if !is_matching_last_mod(&self.settings, &last_mod) {
            return None;
        }

        let file_result = FileResult::with_path(file_path, file_type.clone(), file_size, last_mod);
        Some(file_result)
    }

    fn filter_file_path_to_file_result(&self, file_path: &Path) -> Option<FileResult> {
        if !is_matching_file_path(&self.settings, &file_path) {
            return None;
        }
        let file_type = self.file_types.get_file_type_for_path(&file_path);
        if file_type == FileType::Archive {
            self.filter_archive_file_path_to_file_result(file_path)
        } else {
            self.filter_reg_file_path_to_file_result(file_path, &file_type)
        }
    }

    fn filter_file_paths_to_file_results(&self, file_paths: &Vec<PathBuf>) -> Vec<FileResult> {
        file_paths.iter()
            .map(|p| self.filter_file_path_to_file_result(p))
            .filter(|p| p.is_some())
            .map(|p| p.unwrap()).collect()
    }

    fn rec_find_path(&self, dir_path: &PathBuf, min_depth: i32, max_depth: i32,
                     current_depth: i32) -> Result<Vec<FileResult>, FindError> {
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
            if dir_entry.path().is_dir() && recurse
                && is_traversable_dir_path(&self.settings, &dir_entry.path()) {
                path_dirs.push(dir_entry.path());
            } else if dir_entry.path().is_file() && (min_depth < 0 || current_depth >= min_depth) {
                path_files.push(dir_entry.path());
            }
        }

        if !path_files.is_empty() {
            let path_file_results = self.filter_file_paths_to_file_results(&path_files);
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
            if is_traversable_dir_path(&self.settings, &path_buf) {
                let max_depth =
                    if self.settings.recursive() { self.settings.max_depth() }
                    else { 1 };
                return self.rec_find_path(&path_buf, self.settings.min_depth(), max_depth, 1);
            } else {
                return Err(FindError::new("Startpath does not match find settings"))
            }
        } else if path_buf.is_file() {
            if self.settings.min_depth() > 0 {
                return Ok(file_results);
            }
            match self.filter_file_path_to_file_result(path_buf.as_path()) {
                Some(file_result) => file_results.push(file_result),
                None => return Err(FindError::new("Startpath does not match find settings"))
            }
        } else {
            return Err(FindError::new("Startpath does not match find settings"))
        }
        Ok(file_results)
    }

    /// Initiate a find session for the given settings and get the matching files
    pub fn find(&self) -> Result<Vec<FileResult>, FindError> {
        let mut file_results: Vec<FileResult> = Vec::new();
        for p in self.settings.paths().iter() {
            file_results.extend(self.find_path(p).unwrap_or(Vec::new()));
        }
        if file_results.len() > 1 {
            let file_result_sorter = FileResultSorter::new(self.settings.clone());
            file_result_sorter.sort(&mut file_results);
        }
        Ok(file_results)
    }
}

/// Get the unique list of directories for matching files
pub fn get_matching_dir_paths(file_results: &[FileResult]) -> Vec<PathBuf> {
    let mut dir_set: HashSet<String> = HashSet::new();
    let mut dirs: Vec<PathBuf> = Vec::new();
    for f in file_results.iter() {
        if !dir_set.contains(f.parent()) {
            dirs.push(PathBuf::from(f.parent_path()));
            dir_set.insert(String::from(f.parent()));
        }
    }
    dirs
}

pub fn print_matching_dirs(file_results: &Vec<FileResult>, formatter: &FileResultFormatter) {
    let dirs = get_matching_dir_paths(file_results);
    if dirs.is_empty() {
        log("\nMatching directories: 0");
    } else {
        log(format!("\nMatching directories ({}):", dirs.len()).as_str());
        for dir in dirs.iter() {
            log(format!("{}", (&formatter.format_dir_path)(dir, &formatter.settings)).as_str());
        }
    }
}

pub fn print_matching_files(file_results: &Vec<FileResult>, formatter: &FileResultFormatter) {
    if file_results.is_empty() {
        log("\nMatching files: 0");
    } else {
        log(format!("\nMatching files ({}):", file_results.len()).as_str());
        for fr in file_results.iter() {
            log(format!("{}", formatter.format_file_result(fr)).as_str());
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::time::SystemTime;

    use super::*;
    use crate::config::Config;
    use crate::filetypes::FileType;

    fn get_default_test_settings() -> FindSettings {
        let mut settings = FindSettings::default();
        settings.add_path(String::from("."));
        settings
    }

    #[test]
    fn test_is_matching_dir_path() {
        let mut settings = get_default_test_settings();
        settings.add_out_dir_pattern(String::from("temp"));

        let dir_path = Path::new(".");
        assert!(is_matching_dir_path(&settings, &dir_path));

        let dir_path = Path::new(".git");
        assert!(is_matching_dir_path(&settings, &dir_path));

        let dir_path = Path::new("./temp/");
        assert!(is_matching_dir_path(&settings, &dir_path));
    }

    #[test]
    fn test_is_matching_file_name() {
        let mut settings = get_default_test_settings();
        settings.add_out_file_pattern(String::from("temp"));

        let file_name = String::from("finder.rs");
        assert!(is_matching_file_name(&settings, &file_name));

        let file_name = String::from(".gitignore");
        assert!(is_matching_file_name(&settings, &file_name));

        let file_name = String::from("tempfile.rs");
        assert!(is_matching_file_name(&settings, &file_name));
    }

    #[test]
    fn test_is_matching_file_size() {
        let mut settings = get_default_test_settings();
        settings.set_min_size(1000u64);
        settings.set_max_size(5000u64);

        // in between
        let file_size = 3000u64;
        assert!(is_matching_file_size(&settings, &file_size));

        // matches min size
        let file_size = 1000u64;
        assert!(is_matching_file_size(&settings, &file_size));

        // below min size
        let file_size = 100u64;
        assert!(is_matching_file_size(&settings, &file_size));

        // matches max size
        let file_size = 5000u64;
        assert!(is_matching_file_size(&settings, &file_size));

        // below min size
        let file_size = 5500u64;
        assert!(is_matching_file_size(&settings, &file_size));
    }

    #[test]
    fn test_is_matching_file_path() {
        let mut settings = get_default_test_settings();
        settings.add_in_extension(String::from("js,ts"));
        settings.add_out_dir_pattern(String::from("temp"));
        settings.add_out_file_pattern(String::from("temp"));
        let finder = Finder::new(settings).ok().unwrap();

        // js extension
        let file_path = Path::new("./codefile.js");
        assert!(finder.filter_file_path_to_file_result(&file_path).is_some());

        // ts extension
        let file_path = Path::new("./codefile.ts");
        assert!(finder.filter_file_path_to_file_result(&file_path).is_some());

        // "temp" in path
        let file_path = Path::new("./temp/codefile.ts");
        assert!(finder.filter_file_path_to_file_result(&file_path).is_none());

        // hidden path
        let file_path = Path::new("./.hidden/codefile.ts");
        assert!(finder.filter_file_path_to_file_result(&file_path).is_none());

        // hidden file name
        let file_path = Path::new("./.codefile.ts");
        assert!(finder.filter_file_path_to_file_result(&file_path).is_none());

        // archive file
        let file_path = Path::new("./archive.zip");
        let file_type = FileType::Archive;
        assert!(finder.filter_file_path_to_file_result(&file_path).is_none());

        // archive file + include_archives
        let mut settings = get_default_test_settings();
        settings.set_include_archives(true);
        let finder = Finder::new(settings).ok().unwrap();
        assert!(finder.filter_file_path_to_file_result(&file_path).is_some());
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
        assert!(file_results.len() < 4);
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
        assert!(file_results.len() < 4);
    }
}
