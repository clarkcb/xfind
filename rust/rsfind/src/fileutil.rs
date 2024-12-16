use std::ffi::OsStr;
// use std::{fs, io};
use std::path::{Path, PathBuf};

pub struct FileUtil {}

impl FileUtil {
    /// Expand a file path if it starts with tilde
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Path::new("/home/user"), FileUtil::expand_path(Path::new("~")));
    /// assert_eq!(Path::new("/other/path"), FileUtil::expand_path(Path::new("/other/path")));
    /// ```
    pub fn expand_path(path: &Path) -> PathBuf {
        let path_str = path.to_str().unwrap();
        if path_str.starts_with("~") {
            let user_path = std::env::home_dir().unwrap();
            if path_str.eq("~") || path_str.eq("~/") {
                return user_path;
            }
            if path.starts_with("~/") {
                return user_path.join(path.strip_prefix("~/").unwrap());
            }

            // Another user's home directory
            let home_path = user_path.parent().unwrap().to_path_buf();
            return home_path.join(path_str[1..].to_string());
        }
        PathBuf::from(path)
    }

    /// Expand a file path string if it starts with tilde
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!("/home/user", FileUtil::expand_path_string("~"));
    /// assert_eq!("/other/path", FileUtil::expand_path_string("/other/path"));
    /// ```
    pub fn expand_path_string(file_path: &str) -> String {
        let expanded_path = FileUtil::expand_path(Path::new(file_path));
        expanded_path.to_str().unwrap().to_string()
    }

    pub fn get_extension_from_path(path: &Path) -> Option<&str> {
        path.extension().and_then(OsStr::to_str)
    }

    /// Get a file name's extension, if it exists
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(FileUtil::get_extension("filename.txt"), Some("txt"));
    /// ```
    pub fn get_extension(file_name: &str) -> Option<&str> {
        FileUtil::get_extension_from_path(Path::new(file_name))
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

    /// Check whether a Path is for a hidden dir/file
    pub fn is_hidden_path(file_path: &Path) -> bool {
        for elem in file_path.iter() {
            let elem_string = elem.to_str().unwrap().to_string();
            if elem_string.starts_with('.') && !FileUtil::is_dot_dir(&elem_string) {
                return true;
            }
        }
        false
    }

    /// Check whether a file path string is for a hidden dir/file
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(FileUtil::is_hidden(".git"));
    /// assert!(FileUtil::is_hidden(".gitignore"));
    /// assert!(!FileUtil::is_dot_dir("temp"));
    /// ```
    pub fn is_hidden(file_path: &str) -> bool {
        Self::is_hidden_path(Path::new(file_path))
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
        let home_str = std::env::var("HOME").unwrap();
        let home_path = Path::new(&home_str);
        assert_eq!(home_path, FileUtil::expand_path(Path::new("~")));
        assert_eq!(home_path, FileUtil::expand_path(Path::new("~cary")));
        let xfind_str = format!("{}/src/xfind", home_str);
        let xfind_path = Path::new(&xfind_str);
        assert_eq!(xfind_path, FileUtil::expand_path(Path::new("~/src/xfind")));
        let abs_path = Path::new("/path/to/dir");
        assert_eq!(abs_path, FileUtil::expand_path(abs_path));
    }

    #[test]
    fn test_expand_path_string() {
        let home_str = std::env::var("HOME").unwrap();
        assert_eq!(home_str, FileUtil::expand_path_string("~"));
        let xfind_path = format!("{}/src/xfind", home_str);
        assert_eq!(xfind_path, FileUtil::expand_path_string("~/src/xfind"));
        assert_eq!(String::from("/path/to/dir"), FileUtil::expand_path_string("/path/to/dir"));
    }
}
