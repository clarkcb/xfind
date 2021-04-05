use std::ffi::OsStr;
use std::path::Path;

pub struct FileUtil {}

impl FileUtil {
    /// Get a filename's extension, if it exists
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(FileUtil::get_extension("filename.txt"), Some("txt"));
    /// ```
    pub fn get_extension(filename: &str) -> Option<&str> {
        Path::new(filename).extension().and_then(OsStr::to_str)
    }

    /// Check whether a dirname is for a "dot dir"
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(FileUtil::is_dot_dir("."));
    /// assert!(FileUtil::is_dot_dir(".."));
    /// assert!(!FileUtil::is_dot_dir(".git"));
    /// ```
    pub fn is_dot_dir(dirname: &str) -> bool {
        dirname == "." || dirname == "./" || dirname == ".." || dirname == "../"
    }

    /// Check whether a filepath is for a hidden dir/file
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(FileUtil::is_hidden(".git"));
    /// assert!(FileUtil::is_hidden(".gitignore"));
    /// assert!(!FileUtil::is_dot_dir("temp"));
    /// ```
    pub fn is_hidden(filepath: &str) -> bool {
        for elem in Path::new(filepath).iter() {
            let elem_string = elem.to_str().unwrap().to_string();
            if elem_string.starts_with('.') && !FileUtil::is_dot_dir(&elem_string) {
                return true;
            }
        }
        false
    }

    /// Expand a filepath if it starts with tilde
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!("/home/user", FileUtil::expand_path("~"));
    /// assert_eq!("/other/path", FileUtil::expand_path("/other/path"));
    /// ```
    pub fn expand_path(filepath: &str) -> String {
        if filepath.starts_with("~") {
            let home = std::env::var("HOME").unwrap();
            let expanded = filepath.replacen("~", &home, 1);
            return expanded;
        }
        String::from(filepath)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_extension() {
        let filename = "filename.txt";
        assert_eq!(FileUtil::get_extension(filename), Some("txt"));
        let filename = "filename.";
        assert_eq!(FileUtil::get_extension(filename), Some(""));
        let filename = "filename";
        assert_eq!(FileUtil::get_extension(filename), None);
        let filename = ".filename.txt";
        assert_eq!(FileUtil::get_extension(filename), Some("txt"));
        let filename = ".filename.";
        assert_eq!(FileUtil::get_extension(filename), Some(""));
        let filename = ".filename";
        assert_eq!(FileUtil::get_extension(filename), None);
    }

    #[test]
    fn test_is_dot_dir() {
        let dirname = ".";
        assert!(FileUtil::is_dot_dir(dirname));
        let dirname = "./";
        assert!(FileUtil::is_dot_dir(dirname));
        let dirname = "..";
        assert!(FileUtil::is_dot_dir(dirname));
        let dirname = "../";
        assert!(FileUtil::is_dot_dir(dirname));
        let dirname = ".git";
        assert!(!FileUtil::is_dot_dir(dirname));
    }

    #[test]
    fn test_is_hidden() {
        let filename = "filename.txt";
        assert!(!FileUtil::is_hidden(filename));
        let filename = ".filename.txt";
        assert!(FileUtil::is_hidden(filename));
        let filename = ".filename";
        assert!(FileUtil::is_hidden(filename));
        let filename = ".";
        assert!(!FileUtil::is_hidden(filename));
        let filename = "./";
        assert!(!FileUtil::is_hidden(filename));
        let filename = "..";
        assert!(!FileUtil::is_hidden(filename));
        let filename = "../";
        assert!(!FileUtil::is_hidden(filename));
    }

    #[test]
    fn test_expand_path() {
        let home = std::env::var("HOME").unwrap();
        assert_eq!(home, FileUtil::expand_path("~"));
        let xfindpath = format!("{}/xfind", home);
        assert_eq!(xfindpath, FileUtil::expand_path("~/xfind"));
        assert_eq!(String::from("/path/to/dir"), FileUtil::expand_path("/path/to/dir"));
    }
}
