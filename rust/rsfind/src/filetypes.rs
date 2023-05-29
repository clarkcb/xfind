use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

use serde::{Deserialize, Serialize};

use crate::config::Config;
use crate::fileutil::FileUtil;
use crate::finderror::FindError;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum FileType {
    Unknown,
    Archive,
    Binary,
    Code,
    Text,
    Xml,
}

#[derive(Debug)]
pub struct FileTypes {
    pub file_type_ext_map: HashMap<String, HashSet<String>>,
    pub file_type_name_map: HashMap<String, HashSet<String>>,
}

#[derive(Serialize, Deserialize)]
pub struct JsonFileType {
    r#type: String,
    extensions: Vec<String>,
    names: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub struct JsonFileTypes {
    pub filetypes: Vec<JsonFileType>,
}

impl FileTypes {
    pub fn new() -> Result<FileTypes, FindError> {
        let config = Config::new();
        let contents: String = match fs::read_to_string(config.file_types_path) {
            Ok(contents) => contents,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        let jft: JsonFileTypes = match serde_json::from_str(&contents) {
            Ok(deserialized) => deserialized,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        let mut file_types = FileTypes {
            file_type_ext_map: HashMap::new(),
            file_type_name_map: HashMap::new(),
        };
        for json_filetype in jft.filetypes.iter() {
            let extset: HashSet<String> = json_filetype.extensions.iter().cloned().collect();
            file_types
                .file_type_ext_map
                .insert(json_filetype.r#type.clone(), extset);
            let nameset: HashSet<String> = json_filetype.names.iter().cloned().collect();
            file_types
                .file_type_name_map
                .insert(json_filetype.r#type.clone(), nameset);
            }
        Ok(file_types)
    }

    /// Get a FileType for a given filename
    ///
    /// # Examples
    ///
    /// ```
    /// let file_types = FileTypes::new();
    /// let file_name = "codefile.rs";
    /// let file_type = file_types.get_file_type(file_name);
    ///
    /// assert_eq!(file_type, FileType::Code);
    /// ```
    pub fn get_file_type(&self, file_name: &str) -> FileType {
        if self.is_code_file(file_name) {
            return FileType::Code;
        }
        if self.is_xml_file(file_name) {
            return FileType::Xml;
        }
        if self.is_text_file(file_name) {
            return FileType::Text;
        }
        if self.is_binary_file(file_name) {
            return FileType::Binary;
        }
        if self.is_archive_file(file_name) {
            return FileType::Archive;
        }
        FileType::Unknown
    }

    /// Get a FileType for a given type name
    ///
    /// # Examples
    ///
    /// ```
    /// let file_type = file_type_for_name("binary");
    ///
    /// assert_eq!(file_type, FileType::Binary);
    /// ```
    pub fn file_type_for_name(name: &str) -> FileType {
        match name.to_ascii_lowercase().as_str() {
            "archive" => FileType::Archive,
            "binary" => FileType::Binary,
            "code" => FileType::Code,
            "text" => FileType::Text,
            "xml" => FileType::Xml,
            _ => FileType::Unknown,
        }
    }

    fn is_file_type(&self, type_name: &str, file_name: &str) -> bool {
        let has_ext = match FileUtil::get_extension(&file_name) {
            Some(ext) => self.file_type_ext_map.get(type_name).unwrap().contains(ext),
            None => false,
        };
        if has_ext {
            return true
        }
        self.file_type_name_map.get(type_name).unwrap().contains(file_name)
    }

    pub fn is_archive_file(&self, file_name: &str) -> bool {
        self.is_file_type("archive", file_name)
    }

    pub fn is_binary_file(&self, file_name: &str) -> bool {
        self.is_file_type("binary", file_name)
    }

    pub fn is_code_file(&self, file_name: &str) -> bool {
        self.is_file_type("code", file_name)
    }

    pub fn is_xml_file(&self, file_name: &str) -> bool {
        self.is_file_type("xml", file_name)
    }

    pub fn is_text_file(&self, file_name: &str) -> bool {
        self.is_file_type("text", file_name)
            || self.is_file_type("code", file_name)
            || self.is_file_type("xml", file_name)
    }

    pub fn is_unknown_file(&self, file_name: &str) -> bool {
        self.get_file_type(file_name) == FileType::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_file_type_archive_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "archive.zip";
        assert!(file_types.is_archive_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Archive);
        let file_name = "archive.tar.gz";
        assert!(file_types.is_archive_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Archive);
        let file_name = "archive.tar";
        assert!(file_types.is_archive_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Archive);
        let file_name = "archive.bz2";
        assert!(file_types.is_archive_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Archive);
        let file_name = "archive.Z";
        assert!(file_types.is_archive_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Archive);
    }

    #[test]
    fn get_file_type_binary_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "binary.exe";
        assert!(file_types.is_binary_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Binary);
        let file_name = "binary.o";
        assert!(file_types.is_binary_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Binary);
        let file_name = "binary.dylib";
        assert!(file_types.is_binary_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Binary);
        let file_name = "binary.so";
        assert!(file_types.is_binary_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Binary);
    }

    #[test]
    fn get_file_type_text_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "text.txt";
        assert!(file_types.is_text_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Text);
        let file_name = "text.md";
        assert!(file_types.is_text_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Text);
        let file_name = "text.rtf";
        assert!(file_types.is_text_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Text);
    }

    #[test]
    fn get_file_type_code_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "code.c";
        assert!(file_types.is_code_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Code);
        let file_name = "code.html";
        assert!(file_types.is_code_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Code);
        let file_name = "code.rs";
        assert!(file_types.is_code_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Code);
        let file_name = "code.swift";
        assert!(file_types.is_code_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Code);
    }

    #[test]
    fn get_file_type_xml_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "markup.xml";
        assert!(file_types.is_xml_file(&file_name));
        assert_eq!(file_types.get_file_type(&file_name), FileType::Xml);
    }

    #[test]
    fn get_file_type_unknown_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "unknown.xyz";
        assert!(file_types.is_unknown_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Unknown);
    }
}
