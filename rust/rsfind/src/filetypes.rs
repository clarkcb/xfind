use std::path::Path;

use cached::proc_macro::cached;
use cached::UnboundCache;
use serde::{Deserialize, Serialize};
use sqlite::State;

use crate::config::Config;
use crate::fileutil::FileUtil;
use crate::finderror::FindError;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum FileType {
    Unknown,
    Archive,
    Audio,
    Binary,
    Code,
    Font,
    Image,
    Text,
    Video,
    Xml,
}

// #[derive(Debug)]
pub struct FileTypes {
    pub db: sqlite::Connection,
    pub file_types: Vec<FileType>,
    // pub ext_file_type_cache: HashMap<String, FileType>,
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

fn db_file_type_for_query_and_elem(db: &sqlite::Connection, query: &str, elem: &str) -> usize {
    let mut stmt = db.prepare(query).unwrap();
    stmt.bind((1, elem)).unwrap();
    match stmt.next() {
        Ok(State::Row) => {
            let file_type_id = stmt.read::<i64, _>("file_type_id").unwrap() - 1;
            file_type_id as usize
        },
        _ => 0usize,
    }
}

pub fn db_file_type_for_file_name(db: &sqlite::Connection, file_name: &str) -> usize {
    let query = "SELECT file_type_id FROM file_name WHERE name=?";
    db_file_type_for_query_and_elem(&db, query, file_name)
}

#[cached(
    ty = "UnboundCache<String, usize>",
    create = "{ UnboundCache::with_capacity(64) }",
    convert = r#"{ format!("{}", file_ext) }"#
)]
fn db_file_type_for_extension(db: &sqlite::Connection, file_ext: &str) -> usize {
    let query = "SELECT file_type_id FROM file_extension WHERE extension=?";
    db_file_type_for_query_and_elem(&db, query, file_ext)
}

impl FileTypes {
    pub fn new() -> Result<FileTypes, FindError> {
        let config = Config::new();
        let file_types = FileTypes {
            // TODO: use Connection::open_with_flags to open readonly
            db: sqlite::open(config.xfind_db_path).unwrap(),
            file_types: vec![
                FileType::Unknown,
                FileType::Archive,
                FileType::Audio,
                FileType::Binary,
                FileType::Code,
                FileType::Font,
                FileType::Image,
                FileType::Text,
                FileType::Video,
                FileType::Xml
            ],
            // ext_file_type_cache: HashMap::new()
        };
        Ok(file_types)
    }

    pub fn get_file_type_for_file_name(&self, file_name: &str) -> FileType {
        let file_type_id = db_file_type_for_file_name(&self.db, file_name);
        self.file_types[file_type_id]
    }

    pub fn get_file_type_for_extension(&self, file_ext: &str) -> FileType {
        let file_type_id = db_file_type_for_extension(&self.db, file_ext);
        self.file_types[file_type_id]
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
        let file_type = self.get_file_type_for_file_name(&file_name);
        if file_type != FileType::Unknown {
            file_type
        } else {
            match FileUtil::get_extension(&file_name) {
                Some(ext) => self.get_file_type_for_extension(ext),
                None => FileType::Unknown,
            }
        }
    }

    pub fn get_file_type_for_path(&self, file_path: &Path) -> FileType {
        self.get_file_type(file_path.file_name().unwrap().to_str().unwrap())
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
            "audio" => FileType::Audio,
            "binary" => FileType::Binary,
            "code" => FileType::Code,
            "font" => FileType::Font,
            "image" => FileType::Image,
            "text" => FileType::Text,
            "video" => FileType::Video,
            "xml" => FileType::Xml,
            _ => FileType::Unknown,
        }
    }

    pub fn is_archive_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Archive
    }

    pub fn is_audio_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Audio
    }

    pub fn is_binary_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Binary
    }

    pub fn is_code_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Code
    }

    pub fn is_font_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Font
    }

    pub fn is_image_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Image
    }

    pub fn is_text_file(&self, file_name: &str) -> bool {
        let file_type = self.get_file_type(&file_name);
        file_type == FileType::Text
            || file_type == FileType::Code
            || file_type == FileType::Xml
    }

    pub fn is_video_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Video
    }

    pub fn is_xml_file(&self, file_name: &str) -> bool {
        self.get_file_type(&file_name) == FileType::Xml
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
    fn get_file_type_audio_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "music.mp3";
        assert!(file_types.is_audio_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Audio);
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
    fn get_file_type_font_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "font.ttf";
        assert!(file_types.is_font_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Font);
    }

    #[test]
    fn get_file_type_image_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "image.png";
        assert!(file_types.is_image_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Image);
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
    fn get_file_type_video_file() {
        let file_types = FileTypes::new().ok().unwrap();
        let file_name = "movie.mp4";
        assert!(file_types.is_video_file(file_name));
        assert_eq!(file_types.get_file_type(file_name), FileType::Video);
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
