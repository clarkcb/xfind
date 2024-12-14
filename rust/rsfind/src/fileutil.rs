use std::ffi::OsStr;
use std::{fs, io};
use std::path::{Path, PathBuf};

pub struct FileUtil {}

impl FileUtil {
    pub fn exists(file_path: &str) -> bool {
        let metadata = fs::metadata(&file_path);
        if metadata.is_err() && metadata.err().unwrap().kind() == io::ErrorKind::NotFound {
            false
        } else {
            true
        }
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

    /// Check whether a str file path is for a hidden dir/file
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

    pub fn expand_path(path: &Path) -> PathBuf {
        if path.starts_with("~") {
            let user_path = std::env::home_dir().unwrap();
            let path_string = path.to_str().unwrap().to_string();
            if path_string.eq("~") || path_string.eq("~/") {
                return user_path;
            }
            if path.starts_with("~/") {
                return user_path.join(path.strip_prefix("~/").unwrap());
            }

            // Another user's home directory
            let home_path = user_path.parent().unwrap().to_path_buf();

            return match path_string.find('/') {
                None => {
                    let user_name = &path_string[1..];
                    home_path.join(user_name)
                },
                Some(sep_index) => {
                    let user_name = &path_string[1..sep_index];
                    home_path.join(user_name).join(path_string[sep_index + 1..].trim())
                },
            }
        }
        PathBuf::from(path)
    }

    /// Expand a file path if it starts with tilde
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!("/home/user", FileUtil::expand_path("~"));
    /// assert_eq!("/other/path", FileUtil::expand_path("/other/path"));
    /// ```
    pub fn expand(file_path: &str) -> String {
        let expanded_path = FileUtil::expand_path(Path::new(file_path));
        expanded_path.to_str().unwrap().to_string()
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
        assert_eq!(home, FileUtil::expand("~"));
        let xfindpath = format!("{}/xfind", home);
        assert_eq!(xfindpath, FileUtil::expand("~/xfind"));
        assert_eq!(String::from("/path/to/dir"), FileUtil::expand("/path/to/dir"));
    }
}
