use std::cmp::Ordering;
use std::path::Path;

use crate::filetypes::FileType;

#[derive(Clone, Debug, Eq)]
pub struct FindFile {
    pub containers: Vec<String>,
    pub path: String,
    pub name: String,
    pub filetype: FileType,
}

impl FindFile {
    pub fn new(path: String, name: String, filetype: FileType) -> FindFile {
        FindFile::with_containers(Vec::new(), path, name, filetype)
    }

    pub fn with_containers(
        containers: Vec<String>,
        path: String,
        name: String,
        filetype: FileType,
    ) -> FindFile {
        FindFile {
            containers: containers,
            path: path,
            name: name,
            filetype: filetype,
        }
    }

    pub fn filepath(&self) -> String {
        format!("{}", Path::new(&self.path).join(&self.name).display())
    }

    pub fn fullpath(&self) -> String {
        if self.containers.is_empty() {
            format!("{}", Path::new(&self.path).join(&self.name).display())
        } else {
            let container_str = self.containers.join("!");
            format!(
                "{}!{}",
                container_str,
                Path::new(&self.path).join(&self.name).display()
            )
        }
    }
}

impl Ord for FindFile {
    fn cmp(&self, other: &Self) -> Ordering {
        self.filepath().cmp(&other.filepath())
    }
}

impl PartialOrd for FindFile {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for FindFile {
    fn eq(&self, other: &Self) -> bool {
        self.filepath() == other.filepath()
    }
}

#[cfg(test)]
mod tests {
    use crate::filetypes::FileType;

    use super::*;

    #[test]
    fn test_find_file_abs_path() {
        let sf = FindFile::new(
            "~/src/xfind/rust/rsfind/src".to_string(),
            "finder.rs".to_string(),
            FileType::Code,
        );
        assert_eq!(
            sf.filepath(),
            "~/src/xfind/rust/rsfind/src/finder.rs"
        );
    }

    #[test]
    fn test_find_file_rel_path() {
        let sf = FindFile::new(".".to_string(), "finder.rs".to_string(), FileType::Code);
        assert_eq!(sf.filepath(), "./finder.rs");
    }
}
