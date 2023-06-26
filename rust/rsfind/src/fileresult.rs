use std::path::Path;

use crate::filetypes::FileType;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct FileResult {
    pub containers: Vec<String>,
    pub path: String,
    pub file_name: String,
    pub file_type: FileType,
    pub file_size: u64,
    pub mod_time: u64,
}

impl FileResult {
    pub fn new(path: String, file_name: String, file_type: FileType, file_size: u64, mod_time: u64) -> FileResult {
        FileResult::with_containers(Vec::new(), path, file_name, file_type, file_size, mod_time)
    }

    pub fn with_containers(
        containers: Vec<String>,
        path: String,
        file_name: String,
        file_type: FileType,
        file_size: u64,
        mod_time: u64,
    ) -> FileResult {
        FileResult {
            containers,
            path,
            file_name,
            file_type,
            file_size,
            mod_time,
        }
    }

    pub fn file_path(&self) -> String {
        format!("{}", Path::new(&self.path).join(&self.file_name).display())
    }

    pub fn full_path(&self) -> String {
        if self.containers.is_empty() {
            format!("{}", Path::new(&self.path).join(&self.file_name).display())
        } else {
            let container_str = self.containers.join("!");
            format!(
                "{}!{}",
                container_str,
                Path::new(&self.path).join(&self.file_name).display()
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::SystemTime;
    use crate::filetypes::FileType;

    use super::*;

    #[test]
    fn test_find_file_abs_path() {
        let mod_time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(duration) => duration.as_secs(),
            Err(_error) => 0,
        };
        let fr = FileResult::new(
            "~/src/xfind/rust/rsfind/src".to_string(),
            "finder.rs".to_string(),
            FileType::Code,
            1000,
            mod_time
        );
        assert_eq!(
            fr.file_path(),
            "~/src/xfind/rust/rsfind/src/finder.rs"
        );
    }

    #[test]
    fn test_find_file_rel_path() {
        let fr = FileResult::new(
            ".".to_string(),
            "finder.rs".to_string(),
            FileType::Code,
            1000,
            0
        );
        assert_eq!(fr.file_path(), "./finder.rs");
    }
}
