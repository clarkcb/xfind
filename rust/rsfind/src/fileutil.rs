use std::ffi::OsStr;
use std::path::Path;

pub struct FileUtil {}

impl FileUtil {
    /// Get a file name's extension, if it exists
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(FileUtil::get_extension("filename.txt"), Some("txt"));
    /// ```
    pub fn get_extension(file_name: &str) -> Option<&str> {
        Path::new(file_name).extension().and_then(OsStr::to_str)
    }

    /// Check whether a dir name is for a "dot dir"
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(FileUtil::is_dot_dir("."));
    /// assert!(FileUtil::is_dot_dir(".."));
    /// assert!(!FileUtil::is_dot_dir(".git"));
    /// ```
    pub fn is_dot_dir(dir_name: &str) -> bool {
        dir_name == "." || dir_name == "./" || dir_name == ".." || dir_name == "../"
    }

    /// Check whether a file path is for a hidden dir/file
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(FileUtil::is_hidden(".git"));
    /// assert!(FileUtil::is_hidden(".gitignore"));
    /// assert!(!FileUtil::is_dot_dir("temp"));
    /// ```
    pub fn is_hidden(file_path: &str) -> bool {
        for elem in Path::new(file_path).iter() {
            let elem_string = elem.to_str().unwrap().to_string();
            if elem_string.starts_with('.') && !FileUtil::is_dot_dir(&elem_string) {
                return true;
            }
        }
        false
    }

    /// Expand a file path if it starts with tilde
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!("/home/user", FileUtil::expand_path("~"));
    /// assert_eq!("/other/path", FileUtil::expand_path("/other/path"));
    /// ```
    pub fn expand_path(file_path: &str) -> String {
        if file_path.starts_with("~") {
            let home = std::env::var("HOME").unwrap();
            let expanded = file_path.replacen("~", &home, 1);
            return expanded;
        }
        String::from(file_path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_extension() {
        let file_name = "filename.txt";
        assert_eq!(FileUtil::get_extension(file_name), Some("txt"));
        let file_name = "filename.";
        assert_eq!(FileUtil::get_extension(file_name), Some(""));
        let file_name = "filename";
        assert_eq!(FileUtil::get_extension(file_name), None);
        let file_name = ".filename.txt";
        assert_eq!(FileUtil::get_extension(file_name), Some("txt"));
        let file_name = ".filename.";
        assert_eq!(FileUtil::get_extension(file_name), Some(""));
        let file_name = ".filename";
        assert_eq!(FileUtil::get_extension(file_name), None);
    }

    #[test]
    fn test_is_dot_dir() {
        let dir_name = ".";
        assert!(FileUtil::is_dot_dir(dir_name));
        let dir_name = "./";
        assert!(FileUtil::is_dot_dir(dir_name));
        let dir_name = "..";
        assert!(FileUtil::is_dot_dir(dir_name));
        let dir_name = "../";
        assert!(FileUtil::is_dot_dir(dir_name));
        let dir_name = ".git";
        assert!(!FileUtil::is_dot_dir(dir_name));
    }

    #[test]
    fn test_is_hidden() {
        let file_name = "filename.txt";
        assert!(!FileUtil::is_hidden(file_name));
        let file_name = ".filename.txt";
        assert!(FileUtil::is_hidden(file_name));
        let file_name = ".filename";
        assert!(FileUtil::is_hidden(file_name));
        let file_name = ".";
        assert!(!FileUtil::is_hidden(file_name));
        let file_name = "./";
        assert!(!FileUtil::is_hidden(file_name));
        let file_name = "..";
        assert!(!FileUtil::is_hidden(file_name));
        let file_name = "../";
        assert!(!FileUtil::is_hidden(file_name));
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
