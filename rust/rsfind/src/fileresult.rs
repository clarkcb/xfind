use std::path::Path;
use std::path::PathBuf;

use crate::filetypes::FileType;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct FileResult {
    pub containers: Vec<PathBuf>,
    pub file_path: PathBuf,
    pub file_type: FileType,
    pub file_size: u64,
    pub last_mod: u64,
}

impl FileResult {
    pub fn new(file_path: PathBuf, file_type: FileType, file_size: u64, last_mod: u64) -> FileResult {
        FileResult::with_containers(Vec::new(), file_path, file_type, file_size, last_mod)
    }

    pub fn with_path(file_path: impl AsRef<Path>, file_type: FileType, file_size: u64, last_mod: u64) -> Self {
        FileResult {
            containers: Vec::new(),
            file_path: file_path.as_ref().to_owned(),
            file_type,
            file_size,
            last_mod,
        }
    }

    pub fn with_containers(
        containers: Vec<PathBuf>,
        file_path: PathBuf,
        file_type: FileType,
        file_size: u64,
        last_mod: u64,
    ) -> FileResult {
        FileResult {
            containers,
            file_path,
            file_type,
            file_size,
            last_mod,
        }
    }

    pub fn parent(&self) -> &str {
        self.file_path.parent().unwrap().to_str().unwrap()
    }

    pub fn file_name(&self) -> &str {
        self.file_path.file_name().unwrap().to_str().unwrap()
    }

    pub fn file_path(&self) -> String {
        format!("{}", &self.file_path.display())
    }

    pub fn full_path(&self) -> String {
        if self.containers.is_empty() {
            format!("{}", &self.file_path.display())
        } else {
            let mut container_str = String::from("");
            // let container_str = self.containers.join("!");
            self.containers.iter().for_each(|c| {
                container_str.push_str(c.to_str().unwrap());
                container_str.push_str("!")
            });
            format!(
                "{}{}",
                container_str,
                &self.file_path.display()
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::filetypes::FileType;
    use std::path::Path;
    use std::time::SystemTime;

    use super::*;

    #[test]
    fn test_find_file_abs_path() {
        let last_mod = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(duration) => duration.as_secs(),
            Err(_error) => 0,
        };
        let file_path = Path::new("~/src/xfind/rust/rsfind/src/finder.rs");
        let fr = FileResult::with_path(
            file_path,
            FileType::Code,
            1000,
            last_mod
        );
        assert_eq!(
            fr.file_path(),
            "~/src/xfind/rust/rsfind/src/finder.rs"
        );
    }

    #[test]
    fn test_find_file_rel_path() {
        let file_path = Path::new("./finder.rs");
        let fr = FileResult::with_path(
            file_path,
            FileType::Code,
            1000,
            0
        );
        assert_eq!(fr.file_path(), "./finder.rs");
    }
}
