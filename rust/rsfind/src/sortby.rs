#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SortBy {
    FilePath,
    FileName,
    FileSize,
    FileType,
    LastMod,
}

pub fn sort_by_from_name(name: &str) -> SortBy {
    match name.to_ascii_lowercase().as_str() {
        "filename" => SortBy::FileName,
        "name" => SortBy::FileName,
        "filesize" => SortBy::FileSize,
        "size" => SortBy::FileSize,
        "filetype" => SortBy::FileType,
        "type" => SortBy::FileType,
        "lastmod" => SortBy::LastMod,
        _ => SortBy::FilePath,
    }
}

pub fn name_from_sort_by(sort_by: &SortBy) -> String {
    match sort_by {
        SortBy::FileName => String::from("filename"),
        SortBy::FileSize => String::from("filesize"),
        SortBy::FileType => String::from("filetype"),
        SortBy::LastMod => String::from("lastmod"),
        _ => String::from("filepath"),
    }
}
