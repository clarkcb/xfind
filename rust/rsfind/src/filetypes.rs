use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

use serde::{Deserialize, Serialize};

use crate::config::Config;
use crate::fileutil::FileUtil;
use crate::finderror::FindError;
use std::error::Error;

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
    pub filetype_ext_map: HashMap<String, HashSet<String>>,
    pub filetype_name_map: HashMap<String, HashSet<String>>,
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
        let contents: String = match fs::read_to_string(config.filetypes_path) {
            Ok(contents) => contents,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        let jft: JsonFileTypes = match serde_json::from_str(&contents) {
            Ok(deserialized) => deserialized,
            Err(error) => return Err(FindError::new(&error.to_string())),
        };
        let mut filetypes = FileTypes {
            filetype_ext_map: HashMap::new(),
            filetype_name_map: HashMap::new(),
        };
        for json_filetype in jft.filetypes.iter() {
            let extset: HashSet<String> = json_filetype.extensions.iter().cloned().collect();
            filetypes
                .filetype_ext_map
                .insert(json_filetype.r#type.clone(), extset);
            let nameset: HashSet<String> = json_filetype.names.iter().cloned().collect();
            filetypes
                .filetype_name_map
                .insert(json_filetype.r#type.clone(), nameset);
            }
        Ok(filetypes)
    }

    /// Get a FileType for a given filename
    ///
    /// # Examples
    ///
    /// ```
    /// let filetypes = FileTypes::new();
    /// let filename = "codefile.rs";
    /// let filetype = filetypes.get_file_type(filename);
    ///
    /// assert_eq!(filetype, FileType::Code);
    /// ```
    pub fn get_file_type(&self, filename: &str) -> FileType {
        if self.is_code_file(filename) {
            return FileType::Code;
        }
        if self.is_xml_file(filename) {
            return FileType::Xml;
        }
        if self.is_text_file(filename) {
            return FileType::Text;
        }
        if self.is_binary_file(filename) {
            return FileType::Binary;
        }
        if self.is_archive_file(filename) {
            return FileType::Archive;
        }
        FileType::Unknown
    }

    /// Get a FileType for a given type name
    ///
    /// # Examples
    ///
    /// ```
    /// let filetype = file_type_for_name("binary");
    ///
    /// assert_eq!(filetype, FileType::Binary);
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

    fn is_file_type(&self, typename: &str, filename: &str) -> bool {
        let has_ext = match FileUtil::get_extension(&filename) {
            Some(ext) => self.filetype_ext_map.get(typename).unwrap().contains(ext),
            None => false,
        };
        if has_ext {
            return true
        }
        self.filetype_name_map.get(typename).unwrap().contains(filename)
    }

    pub fn is_archive_file(&self, filename: &str) -> bool {
        self.is_file_type("archive", filename)
    }

    pub fn is_binary_file(&self, filename: &str) -> bool {
        self.is_file_type("binary", filename)
    }

    pub fn is_code_file(&self, filename: &str) -> bool {
        self.is_file_type("code", filename)
    }

    pub fn is_xml_file(&self, filename: &str) -> bool {
        self.is_file_type("xml", filename)
    }

    pub fn is_text_file(&self, filename: &str) -> bool {
        self.is_file_type("text", filename)
            || self.is_file_type("code", filename)
            || self.is_file_type("xml", filename)
    }

    pub fn is_unknown_file(&self, filename: &str) -> bool {
        self.get_file_type(filename) == FileType::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_file_type_archive_file() {
        let filetypes = FileTypes::new().ok().unwrap();
        let filename = "archive.zip";
        assert!(filetypes.is_archive_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Archive);
        let filename = "archive.tar.gz";
        assert!(filetypes.is_archive_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Archive);
        let filename = "archive.tar";
        assert!(filetypes.is_archive_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Archive);
        let filename = "archive.bz2";
        assert!(filetypes.is_archive_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Archive);
        let filename = "archive.Z";
        assert!(filetypes.is_archive_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Archive);
    }

    #[test]
    fn get_file_type_binary_file() {
        let filetypes = FileTypes::new().ok().unwrap();
        let filename = "binary.exe";
        assert!(filetypes.is_binary_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Binary);
        let filename = "binary.o";
        assert!(filetypes.is_binary_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Binary);
        let filename = "binary.dylib";
        assert!(filetypes.is_binary_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Binary);
        let filename = "binary.so";
        assert!(filetypes.is_binary_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Binary);
    }

    #[test]
    fn get_file_type_text_file() {
        let filetypes = FileTypes::new().ok().unwrap();
        let filename = "text.txt";
        assert!(filetypes.is_text_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Text);
        let filename = "text.md";
        assert!(filetypes.is_text_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Text);
        let filename = "text.rtf";
        assert!(filetypes.is_text_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Text);
    }

    #[test]
    fn get_file_type_code_file() {
        let filetypes = FileTypes::new().ok().unwrap();
        let filename = "code.c";
        assert!(filetypes.is_code_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Code);
        let filename = "code.html";
        assert!(filetypes.is_code_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Code);
        let filename = "code.rs";
        assert!(filetypes.is_code_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Code);
        let filename = "code.swift";
        assert!(filetypes.is_code_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Code);
    }

    #[test]
    fn get_file_type_xml_file() {
        let filetypes = FileTypes::new().ok().unwrap();
        let filename = "markup.xml";
        assert!(filetypes.is_xml_file(&filename));
        assert_eq!(filetypes.get_file_type(&filename), FileType::Xml);
    }

    #[test]
    fn get_file_type_unknown_file() {
        let filetypes = FileTypes::new().ok().unwrap();
        let filename = "unknown.xyz";
        assert!(filetypes.is_unknown_file(filename));
        assert_eq!(filetypes.get_file_type(filename), FileType::Unknown);
    }
}
