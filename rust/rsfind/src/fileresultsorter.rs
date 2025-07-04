use crate::fileresult::FileResult;
use crate::findsettings::FindSettings;
use crate::sortby::SortBy;

pub struct FileResultSorter {
    pub settings: FindSettings,
}

pub fn cmp_by_path(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let path_cmp = fr1.parent().cmp(&fr2.parent());
    if path_cmp.is_eq() {
        return fr1.file_name().cmp(&fr2.file_name());
    }
    path_cmp
}

pub fn cmp_by_path_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let path_cmp = fr1.parent().to_lowercase().cmp(&fr2.parent().to_lowercase());
    if path_cmp.is_eq() {
        return fr1.file_name().to_lowercase().cmp(&fr2.file_name().to_lowercase());
    }
    path_cmp
}

pub fn cmp_by_name(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let name_cmp = fr1.file_name().cmp(&fr2.file_name());
    if name_cmp.is_eq() {
        return fr1.parent().cmp(&fr2.parent());
    }
    name_cmp
}

pub fn cmp_by_name_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let name_cmp = fr1.file_name().to_lowercase().cmp(&fr2.file_name().to_lowercase());
    if name_cmp.is_eq() {
        return fr1.parent().to_lowercase().cmp(&fr2.parent().to_lowercase());
    }
    name_cmp
}

pub fn cmp_by_size(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let size_cmp = fr1.file_size.cmp(&fr2.file_size);
    if size_cmp.is_eq() {
        return cmp_by_path(fr1, fr2);
    }
    size_cmp
}

pub fn cmp_by_size_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let size_cmp = fr1.file_size.cmp(&fr2.file_size);
    if size_cmp.is_eq() {
        return cmp_by_path_ci(fr1, fr2);
    }
    size_cmp
}

pub fn cmp_by_type(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let type_cmp = fr1.file_type.cmp(&fr2.file_type);
    if type_cmp.is_eq() {
        return cmp_by_path(fr1, fr2);
    }
    type_cmp
}

pub fn cmp_by_type_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let type_cmp = fr1.file_type.cmp(&fr2.file_type);
    if type_cmp.is_eq() {
        return cmp_by_path_ci(fr1, fr2);
    }
    type_cmp
}

pub fn cmp_by_last_mod(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let last_mod_cmp = fr1.last_mod.cmp(&fr2.last_mod);
    if last_mod_cmp.is_eq() {
        return cmp_by_path(fr1, fr2);
    }
    last_mod_cmp
}

pub fn cmp_by_last_mod_ci(fr1: &FileResult, fr2: &FileResult) -> std::cmp::Ordering {
    let last_mod_cmp = fr1.last_mod.cmp(&fr2.last_mod);
    if last_mod_cmp.is_eq() {
        return cmp_by_path_ci(fr1, fr2);
    }
    last_mod_cmp
}

impl FileResultSorter {
    pub fn new(settings: FindSettings) -> FileResultSorter {
        Self {
            settings,
        }
    }

    pub fn get_file_result_comparator(&self) -> impl Fn(&FileResult, &FileResult) -> std::cmp::Ordering + use<> {
        match (self.settings.sort_by(), self.settings.sort_case_insensitive(), self.settings.sort_descending()) {
            (SortBy::FileName, false, false) => cmp_by_name,
            (SortBy::FileName, false, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_name(fr2, fr1),
            (SortBy::FileName, true, false) => cmp_by_name_ci,
            (SortBy::FileName, true, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_name_ci(fr2, fr1),
            (SortBy::FilePath, false, false) => cmp_by_path,
            (SortBy::FilePath, false, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_path(fr2, fr1),
            (SortBy::FilePath, true, false) => cmp_by_path_ci,
            (SortBy::FilePath, true, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_path_ci(fr2, fr1),
            (SortBy::FileSize, false, false) => cmp_by_size,
            (SortBy::FileSize, false, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_size(fr2, fr1),
            (SortBy::FileSize, true, false) => cmp_by_size_ci,
            (SortBy::FileSize, true, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_size_ci(fr2, fr1),
            (SortBy::FileType, false, false) => cmp_by_type,
            (SortBy::FileType, false, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_type(fr2, fr1),
            (SortBy::FileType, true, false) => cmp_by_type_ci,
            (SortBy::FileType, true, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_type_ci(fr2, fr1),
            (SortBy::LastMod, false, false) => cmp_by_last_mod,
            (SortBy::LastMod, false, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_last_mod(fr2, fr1),
            (SortBy::LastMod, true, false) => cmp_by_last_mod_ci,
            (SortBy::LastMod, true, true) => |fr1: &FileResult, fr2: &FileResult| cmp_by_last_mod_ci(fr2, fr1),
        }
    }

    pub fn sort(&self, file_results: &mut Vec<FileResult>) {
        let file_result_comparator = self.get_file_result_comparator();
        file_results.sort_by(file_result_comparator);
    }
}
