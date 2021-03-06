use regex::Regex;

use crate::filetypes;

#[derive(Clone, Debug)]
pub struct FindSettings {
    pub archives_only: bool,
    pub colorize: bool,
    pub debug: bool,
    pub exclude_hidden: bool,
    pub in_archive_extensions: Vec<String>,
    pub in_archive_file_patterns: Vec<Regex>,
    pub in_dir_patterns: Vec<Regex>,
    pub in_extensions: Vec<String>,
    pub in_file_patterns: Vec<Regex>,
    pub in_file_types: Vec<filetypes::FileType>,
    pub include_archives: bool,
    pub list_dirs: bool,
    pub list_files: bool,
    pub out_archive_extensions: Vec<String>,
    pub out_archive_file_patterns: Vec<Regex>,
    pub out_dir_patterns: Vec<Regex>,
    pub out_extensions: Vec<String>,
    pub out_file_patterns: Vec<Regex>,
    pub out_file_types: Vec<filetypes::FileType>,
    pub paths: Vec<String>,
    pub print_usage: bool,
    pub print_version: bool,
    pub recursive: bool,
    pub verbose: bool,
}

impl FindSettings {
    pub fn default() -> FindSettings {
        FindSettings {
            archives_only: false,
            colorize: true,
            debug: false,
            exclude_hidden: true,
            in_archive_extensions: Vec::new(),
            in_archive_file_patterns: Vec::new(),
            in_dir_patterns: Vec::new(),
            in_extensions: Vec::new(),
            in_file_patterns: Vec::new(),
            in_file_types: Vec::new(),
            include_archives: false,
            list_dirs: false,
            list_files: false,
            out_archive_extensions: Vec::new(),
            out_archive_file_patterns: Vec::new(),
            out_dir_patterns: Vec::new(),
            out_extensions: Vec::new(),
            out_file_patterns: Vec::new(),
            out_file_types: Vec::new(),
            paths: Vec::new(),
            print_usage: false,
            print_version: false,
            recursive: true,
            verbose: false,
        }
    }

    pub fn set_archives_only(&mut self, b: bool) {
        self.archives_only = b;
        if b {
            self.include_archives = b;
        }
    }

    pub fn set_debug(&mut self, b: bool) {
        self.debug = b;
        if b {
            self.verbose = b;
        }
    }

    pub fn add_in_archive_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self.in_archive_extensions);
    }

    pub fn add_out_archive_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self.out_archive_extensions);
    }

    pub fn add_in_archive_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self.in_archive_file_patterns);
    }

    pub fn add_out_archive_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self.out_archive_file_patterns);
    }

    pub fn add_in_dir_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self.in_dir_patterns);
    }

    pub fn add_out_dir_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self.out_dir_patterns);
    }

    pub fn add_in_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self.in_extensions);
    }

    pub fn add_out_extension(&mut self, ext: String) {
        add_extensions(ext, &mut self.out_extensions);
    }

    pub fn add_in_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self.in_file_patterns);
    }

    pub fn add_out_file_pattern(&mut self, pattern: String) {
        add_pattern(pattern, &mut self.out_file_patterns);
    }

    pub fn add_in_file_type(&mut self, filetype: filetypes::FileType) {
        self.in_file_types.push(filetype)
    }

    pub fn add_out_file_type(&mut self, filetype: filetypes::FileType) {
        self.out_file_types.push(filetype)
    }

    pub fn add_path(&mut self, path: String) {
        self.paths.push(path)
    }
}

fn add_extensions(new_ext_str: String, extensions: &mut Vec<String>) {
    let v: Vec<&str> = new_ext_str.split(',').filter(|x| !x.is_empty()).collect();
    for x in v.iter() {
        extensions.push(x.to_string());
    }
}

fn add_pattern(pattern: String, patterns: &mut Vec<Regex>) {
    patterns.push(Regex::new(pattern.as_str()).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        let settings = FindSettings::default();
        assert_eq!(settings.archives_only, false);
        assert_eq!(settings.colorize, true);
        assert_eq!(settings.debug, false);
        assert_eq!(settings.exclude_hidden, true);
        assert!(settings.in_archive_extensions.is_empty());
        assert!(settings.in_archive_file_patterns.is_empty());
        assert!(settings.in_dir_patterns.is_empty());
        assert!(settings.in_extensions.is_empty());
        assert!(settings.in_file_patterns.is_empty());
        assert!(settings.in_file_types.is_empty());
        assert_eq!(settings.include_archives, false);
        assert_eq!(settings.list_dirs, false);
        assert_eq!(settings.list_files, false);
        assert!(settings.out_archive_extensions.is_empty());
        assert!(settings.out_archive_file_patterns.is_empty());
        assert!(settings.out_dir_patterns.is_empty());
        assert!(settings.out_extensions.is_empty());
        assert!(settings.out_file_patterns.is_empty());
        assert!(settings.out_file_types.is_empty());
        assert!(settings.paths.is_empty());
        assert_eq!(settings.print_usage, false);
        assert_eq!(settings.print_version, false);
        assert_eq!(settings.recursive, true);
        assert_eq!(settings.verbose, false);
    }

    #[test]
    fn test_set_archives_only() {
        let mut settings = FindSettings::default();
        assert_eq!(settings.archives_only, false);
        assert_eq!(settings.include_archives, false);
        settings.set_archives_only(true);
        assert_eq!(settings.archives_only, true);
        assert_eq!(settings.include_archives, true);
        settings.set_archives_only(false);
        assert_eq!(settings.archives_only, false);
        assert_eq!(settings.include_archives, true);
    }

    #[test]
    fn test_set_debug() {
        let mut settings = FindSettings::default();
        assert_eq!(settings.debug, false);
        assert_eq!(settings.verbose, false);
        settings.set_debug(true);
        assert_eq!(settings.debug, true);
        assert_eq!(settings.verbose, true);
        settings.set_debug(false);
        assert_eq!(settings.debug, false);
        assert_eq!(settings.verbose, true);
    }

    #[test]
    fn test_add_extensions() {
        let mut settings = FindSettings::default();

        settings.add_in_extension("c".to_string());
        assert_eq!(settings.in_extensions.len(), 1);
        assert_eq!(settings.in_extensions[0], "c".to_string());

        settings.add_in_extension("cpp,hs,js".to_string());
        assert_eq!(settings.in_extensions.len(), 4);
        assert_eq!(settings.in_extensions[1], "cpp".to_string());
        assert_eq!(settings.in_extensions[2], "hs".to_string());
        assert_eq!(settings.in_extensions[3], "js".to_string());

        settings.add_in_extension("rs,".to_string());
        assert_eq!(settings.in_extensions.len(), 5);
        assert_eq!(settings.in_extensions[4], "rs".to_string());

        settings.add_in_extension(",ts".to_string());
        assert_eq!(settings.in_extensions.len(), 6);
        assert_eq!(settings.in_extensions[5], "ts".to_string());
    }

    #[test]
    fn test_add_pattern() {
        let mut settings = FindSettings::default();

        settings.add_in_dir_pattern("src".to_string());
        assert_eq!(settings.in_dir_patterns.len(), 1);
        assert_eq!(settings.in_dir_patterns[0].to_string(), "src".to_string());

        settings.add_out_dir_pattern("temp".to_string());
        assert_eq!(settings.out_dir_patterns.len(), 1);
        assert_eq!(settings.out_dir_patterns[0].to_string(), "temp".to_string());

        settings.add_in_file_pattern("find".to_string());
        assert_eq!(settings.in_file_patterns.len(), 1);
        assert_eq!(
            settings.in_file_patterns[0].to_string(),
            "find".to_string()
        );

        settings.add_out_file_pattern("tempfile".to_string());
        assert_eq!(settings.out_file_patterns.len(), 1);
        assert_eq!(
            settings.out_file_patterns[0].to_string(),
            "tempfile".to_string()
        );
    }
}
