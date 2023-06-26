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
        "name" => SortBy::FileName,
        "size" => SortBy::FileSize,
        "type" => SortBy::FileType,
        "lastmod" => SortBy::LastMod,
        _ => SortBy::FilePath,
    }
}

pub fn name_from_sort_by(sort_by: &SortBy) -> String {
    match sort_by {
        SortBy::FileName => String::from("name"),
        SortBy::FileSize => String::from("size"),
        SortBy::FileType => String::from("type"),
        SortBy::LastMod => String::from("lastmod"),
        _ => String::from("path"),
    }
}
