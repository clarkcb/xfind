use std::fmt;
use regex::Regex;

use crate::filetypes;
use crate::sortby::SortBy;

#[derive(Clone)]
pub struct FindSettings {
    _archives_only: bool,
    _debug: bool,
    _in_archive_extensions: Vec<String>,
    _in_archive_file_patterns: Vec<Regex>,
    _in_dir_patterns: Vec<Regex>,
    _in_extensions: Vec<String>,
    _in_file_patterns: Vec<Regex>,
    _in_file_types: Vec<filetypes::FileType>,
    _include_archives: bool,
    _include_hidden: bool,
    _max_depth: i64,
    _max_last_mod: u64,
    _max_size: u64,
    _min_depth: i64,
    _min_last_mod: u64,
    _min_size: u64,
    _out_archive_extensions: Vec<String>,
    _out_archive_file_patterns: Vec<Regex>,
    _out_dir_patterns: Vec<Regex>,
    _out_extensions: Vec<String>,
    _out_file_patterns: Vec<Regex>,
    _out_file_types: Vec<filetypes::FileType>,
    _paths: Vec<String>,
    _print_dirs: bool,
    _print_files: bool,
    _print_usage: bool,
    _print_version: bool,
    _recursive: bool,
    _sort_by: SortBy,
    _sort_case_insensitive: bool,
    _sort_descending: bool,
    _verbose: bool,
}

impl FindSettings {
    pub fn default() -> FindSettings {
        FindSettings {
            _archives_only: false,
            _debug: false,
            _in_archive_extensions: Vec::new(),
            _in_archive_file_patterns: Vec::new(),
            _in_dir_patterns: Vec::new(),
            _in_extensions: Vec::new(),
            _in_file_patterns: Vec::new(),
            _in_file_types: Vec::new(),
            _include_archives: false,
            _include_hidden: false,
            _max_depth: -1i64,
            _max_last_mod: 0u64,
            _max_size: 0u64,
            _min_depth: -1i64,
            _min_last_mod: 0u64,
            _min_size: 0u64,
            _out_archive_extensions: Vec::new(),
            _out_archive_file_patterns: Vec::new(),
            _out_dir_patterns: Vec::new(),
            _out_extensions: Vec::new(),
            _out_file_patterns: Vec::new(),
            _out_file_types: Vec::new(),
            _paths: Vec::new(),
            _print_dirs: false,
            _print_files: false,
            _print_usage: false,
            _print_version: false,
            _recursive: true,
            _sort_by: SortBy::FilePath,
            _sort_case_insensitive: false,
            _sort_descending: false,
            _verbose: false,
        }
    }

    pub fn archives_only(&self) -> bool {
        self._archives_only
    }

    pub fn set_archives_only(&mut self, b: bool) {
        self._archives_only = b;
        if b {
            self._include_archives = b;
        }
    }

    pub fn debug(&self) -> bool {
        self._debug
    }

    pub fn set_debug(&mut self, b: bool) {
        self._debug = b;
        if b {
            self._verbose = b;
        }
    }

    pub fn in_archive_extensions(&self) -> &Vec<String> {
        &self._in_archive_extensions
    }

    pub fn add_in_archive_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self._in_archive_extensions);
    }

    pub fn in_archive_file_patterns(&self) -> &Vec<Regex> {
        &self._in_archive_file_patterns
    }

    pub fn add_in_archive_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._in_archive_file_patterns);
    }

    pub fn in_dir_patterns(&self) -> &Vec<Regex> {
        &self._in_dir_patterns
    }

    pub fn add_in_dir_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._in_dir_patterns);
    }

    pub fn in_extensions(&self) -> &Vec<String> {
        &self._in_extensions
    }

    pub fn add_in_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self._in_extensions);
    }

    pub fn in_file_patterns(&self) -> &Vec<Regex> {
        &self._in_file_patterns
    }

    pub fn add_in_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._in_file_patterns);
    }

    pub fn in_file_types(&self) -> &Vec<filetypes::FileType> {
        &self._in_file_types
    }

    pub fn add_in_file_type(&mut self, file_type: filetypes::FileType) {
        self._in_file_types.push(file_type)
    }

    pub fn include_archives(&self) -> bool {
        self._include_archives
    }

    pub fn set_include_archives(&mut self, b: bool) {
        self._include_archives = b
    }

    pub fn include_hidden(&self) -> bool {
        self._include_hidden
    }

    pub fn set_include_hidden(&mut self, b: bool) {
        self._include_hidden = b
    }

    pub fn max_depth(&self) -> i64 {
        self._max_depth
    }

    pub fn set_max_depth(&mut self, m: i64) {
        self._max_depth = m
    }

    pub fn max_last_mod(&self) -> u64 {
        self._max_last_mod
    }

    pub fn set_max_last_mod(&mut self, m: u64) {
        self._max_last_mod = m
    }

    pub fn max_size(&self) -> u64 {
        self._max_size
    }

    pub fn set_max_size(&mut self, m: u64) {
        self._max_size = m
    }

    pub fn min_depth(&self) -> i64 {
        self._min_depth
    }

    pub fn set_min_depth(&mut self, m: i64) {
        self._min_depth = m
    }

    pub fn min_last_mod(&self) -> u64 {
        self._min_last_mod
    }

    pub fn set_min_last_mod(&mut self, m: u64) {
        self._min_last_mod = m
    }

    pub fn min_size(&self) -> u64 {
        self._min_size
    }

    pub fn set_min_size(&mut self, m: u64) {
        self._min_size = m
    }

    pub fn out_archive_extensions(&self) -> &Vec<String> {
        &self._out_archive_extensions
    }

    pub fn add_out_archive_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self._out_archive_extensions);
    }

    pub fn out_archive_file_patterns(&self) -> &Vec<Regex> {
        &self._out_archive_file_patterns
    }

    pub fn add_out_archive_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._out_archive_file_patterns);
    }

    pub fn out_dir_patterns(&self) -> &Vec<Regex> {
        &self._out_dir_patterns
    }

    pub fn add_out_dir_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._out_dir_patterns);
    }

    pub fn out_extensions(&self) -> &Vec<String> {
        &self._out_extensions
    }

    pub fn add_out_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self._out_extensions);
    }

    pub fn out_file_patterns(&self) -> &Vec<Regex> {
        &self._out_file_patterns
    }

    pub fn add_out_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self._out_file_patterns);
    }

    pub fn out_file_types(&self) -> &Vec<filetypes::FileType> {
        &self._out_file_types
    }

    pub fn add_out_file_type(&mut self, file_type: filetypes::FileType) {
        self._out_file_types.push(file_type)
    }

    pub fn paths(&self) -> &Vec<String> {
        &self._paths
    }

    pub fn add_path(&mut self, path: String) {
        self._paths.push(path)
    }

    pub fn print_dirs(&self) -> bool {
        self._print_dirs
    }

    pub fn set_print_dirs(&mut self, b: bool) {
        self._print_dirs = b
    }

    pub fn print_files(&self) -> bool {
        self._print_files
    }

    pub fn set_print_files(&mut self, b: bool) {
        self._print_files = b
    }

    pub fn print_usage(&self) -> bool {
        self._print_usage
    }

    pub fn set_print_usage(&mut self, b: bool) {
        self._print_usage = b
    }

    pub fn print_version(&self) -> bool {
        self._print_version
    }

    pub fn set_print_version(&mut self, b: bool) {
        self._print_version = b
    }

    pub fn recursive(&self) -> bool {
        self._recursive
    }

    pub fn set_recursive(&mut self, b: bool) {
        self._recursive = b
    }

    pub fn sort_by(&self) -> SortBy {
        self._sort_by
    }

    pub fn set_sort_by(&mut self, s: SortBy) {
        self._sort_by = s
    }

    pub fn sort_case_insensitive(&self) -> bool {
        self._sort_case_insensitive
    }

    pub fn set_sort_case_insensitive(&mut self, b: bool) {
        self._sort_case_insensitive = b
    }

    pub fn sort_descending(&self) -> bool {
        self._sort_descending
    }

    pub fn set_sort_descending(&mut self, b: bool) {
        self._sort_descending = b
    }

    pub fn verbose(&self) -> bool {
        self._verbose
    }

    pub fn set_verbose(&mut self, b: bool) {
        self._verbose = b
    }

    fn get_settings_string(&self) -> String {
        let mut s = String::from("FindSettings(");
        s.push_str(format!("archives_only={}", &self.archives_only()).as_str());
        s.push_str(format!(", debug={}", &self.debug()).as_str());
        s.push_str(format!(", in_archive_extensions={:?}", &self.in_archive_extensions()).as_str());
        s.push_str(format!(", in_archive_file_patterns={}", get_regex_vec_string(&self.in_archive_file_patterns())).as_str());
        s.push_str(format!(", in_dir_patterns={}", get_regex_vec_string(&self.in_dir_patterns())).as_str());
        s.push_str(format!(", in_extensions={:?}", &self.in_extensions()).as_str());
        s.push_str(format!(", in_file_patterns={}", get_regex_vec_string(&self.in_file_patterns())).as_str());
        s.push_str(format!(", in_file_types={:?}", &self.in_file_types()).as_str());
        s.push_str(format!(", include_archives={:?}", &self.include_archives()).as_str());
        s.push_str(format!(", include_hidden={:?}", &self.include_hidden()).as_str());
        s.push_str(format!(", max_depth={:?}", &self.max_depth()).as_str());
        s.push_str(format!(", max_last_mod={:?}", &self.max_last_mod()).as_str());
        s.push_str(format!(", max_size={:?}", &self.max_size()).as_str());
        s.push_str(format!(", min_depth={:?}", &self.min_depth()).as_str());
        s.push_str(format!(", min_last_mod={:?}", &self.min_last_mod()).as_str());
        s.push_str(format!(", min_size={:?}", &self.min_size()).as_str());
        s.push_str(format!(", out_archive_extensions={:?}", &self.out_archive_extensions()).as_str());
        s.push_str(format!(", out_archive_file_patterns={}", get_regex_vec_string(&self.out_archive_file_patterns())).as_str());
        s.push_str(format!(", out_dir_patterns={}", get_regex_vec_string(&self.out_dir_patterns())).as_str());
        s.push_str(format!(", out_extensions={:?}", &self.out_extensions()).as_str());
        s.push_str(format!(", out_file_patterns={}", get_regex_vec_string(&self.out_file_patterns())).as_str());
        s.push_str(format!(", out_file_types={:?}", &self.out_file_types()).as_str());
        s.push_str(format!(", paths={:?}", &self.paths()).as_str());
        s.push_str(format!(", print_dirs={:?}", &self.print_dirs()).as_str());
        s.push_str(format!(", print_files={:?}", &self.print_files()).as_str());
        s.push_str(format!(", print_usage={:?}", &self.print_usage()).as_str());
        s.push_str(format!(", print_version={:?}", &self.print_version()).as_str());
        s.push_str(format!(", recursive={}", &self.recursive()).as_str());
        s.push_str(format!(", sort_by={:?}", &self.sort_by()).as_str());
        s.push_str(format!(", sort_case_insensitive={}", &self.sort_case_insensitive()).as_str());
        s.push_str(format!(", sort_descending={}", &self.sort_descending()).as_str());
        s.push_str(format!(", verbose={}", &self.verbose()).as_str());
        s.push_str(")");
        s
    }
}

impl fmt::Debug for FindSettings {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_settings_string())
    }
}

fn add_extensions(new_ext_str: String, extensions: &mut Vec<String>) {
    let v: Vec<&str> = new_ext_str.split(',').filter(|x| !x.is_empty()).collect();
    for x in v.iter() {
        extensions.push(x.to_string());
    }
}

pub fn add_pattern(pattern: String, patterns: &mut Vec<Regex>) {
    patterns.push(Regex::new(pattern.as_str()).unwrap());
}

fn get_regex_vec_string(vec: &Vec<Regex>) -> String {
    let patterns: Vec<String> = vec.iter().map(|r| r.to_string()).collect();
    let mut s = String::from("[");
    let mut i = 0;
    for pattern in patterns.iter() {
        if i > 0 {
            s.push_str(", ");
        }
        s.push_str(format!("{:?}", pattern).as_str());
        i += 1;
    }
    s.push_str("]");
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let settings = FindSettings::default();
        assert_eq!(settings.archives_only(), false);
        assert_eq!(settings.debug(), false);
        assert!(settings.in_archive_extensions().is_empty());
        assert!(settings.in_archive_file_patterns().is_empty());
        assert!(settings.in_dir_patterns().is_empty());
        assert!(settings.in_extensions().is_empty());
        assert!(settings.in_file_patterns().is_empty());
        assert!(settings.in_file_types().is_empty());
        assert_eq!(settings.include_archives(), false);
        assert_eq!(settings.include_hidden(), false);
        assert_eq!(settings.max_depth(), -1);
        assert_eq!(settings.max_last_mod(), 0);
        assert_eq!(settings.max_size(), 0);
        assert_eq!(settings.min_depth(), -1);
        assert_eq!(settings.min_last_mod(), 0);
        assert_eq!(settings.min_size(), 0);
        assert!(settings.out_archive_extensions().is_empty());
        assert!(settings.out_archive_file_patterns().is_empty());
        assert!(settings.out_dir_patterns().is_empty());
        assert!(settings.out_extensions().is_empty());
        assert!(settings.out_file_patterns().is_empty());
        assert!(settings.out_file_types().is_empty());
        assert!(settings.paths().is_empty());
        assert_eq!(settings.print_dirs(), false);
        assert_eq!(settings.print_files(), false);
        assert_eq!(settings.print_usage(), false);
        assert_eq!(settings.print_version(), false);
        assert_eq!(settings.recursive(), true);
        assert_eq!(settings.sort_by(), SortBy::FilePath);
        assert_eq!(settings.sort_case_insensitive(), false);
        assert_eq!(settings.sort_descending(), false);
        assert_eq!(settings.verbose(), false);
    }

    #[test]
    fn test_set_archives_only() {
        let mut settings = FindSettings::default();
        assert_eq!(settings.archives_only(), false);
        assert_eq!(settings.include_archives(), false);
        settings.set_archives_only(true);
        assert_eq!(settings.archives_only(), true);
        assert_eq!(settings.include_archives(), true);
        settings.set_archives_only(false);
        assert_eq!(settings.archives_only(), false);
        assert_eq!(settings.include_archives(), true);
    }

    #[test]
    fn test_set_debug() {
        let mut settings = FindSettings::default();
        assert_eq!(settings.debug(), false);
        assert_eq!(settings.verbose(), false);
        settings.set_debug(true);
        assert_eq!(settings.debug(), true);
        assert_eq!(settings.verbose(), true);
        settings.set_debug(false);
        assert_eq!(settings.debug(), false);
        assert_eq!(settings.verbose(), true);
    }

    #[test]
    fn test_add_extensions() {
        let mut settings = FindSettings::default();

        settings.add_in_extension("c".to_string());
        assert_eq!(settings.in_extensions().len(), 1);
        assert_eq!(settings.in_extensions()[0], "c".to_string());

        settings.add_in_extension("cpp,hs,js".to_string());
        assert_eq!(settings.in_extensions().len(), 4);
        assert_eq!(settings.in_extensions()[1], "cpp".to_string());
        assert_eq!(settings.in_extensions()[2], "hs".to_string());
        assert_eq!(settings.in_extensions()[3], "js".to_string());

        settings.add_in_extension("rs,".to_string());
        assert_eq!(settings.in_extensions().len(), 5);
        assert_eq!(settings.in_extensions()[4], "rs".to_string());

        settings.add_in_extension(",ts".to_string());
        assert_eq!(settings.in_extensions().len(), 6);
        assert_eq!(settings.in_extensions()[5], "ts".to_string());
    }

    #[test]
    fn test_add_pattern() {
        let mut settings = FindSettings::default();

        settings.add_in_dir_pattern("src".to_string());
        assert_eq!(settings.in_dir_patterns().len(), 1);
        assert_eq!(settings.in_dir_patterns()[0].to_string(), "src".to_string());

        settings.add_out_dir_pattern("temp".to_string());
        assert_eq!(settings.out_dir_patterns().len(), 1);
        assert_eq!(settings.out_dir_patterns()[0].to_string(), "temp".to_string());

        settings.add_in_file_pattern("find".to_string());
        assert_eq!(settings.in_file_patterns().len(), 1);
        assert_eq!(
            settings.in_file_patterns()[0].to_string(),
            "find".to_string()
        );

        settings.add_out_file_pattern("tempfile".to_string());
        assert_eq!(settings.out_file_patterns().len(), 1);
        assert_eq!(
            settings.out_file_patterns()[0].to_string(),
            "tempfile".to_string()
        );
    }
}
