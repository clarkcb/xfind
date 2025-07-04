#include "FileResultSorter.h"

namespace cppfind {
    FileResultSorter::FileResultSorter(const FindSettings& settings) :  m_settings{settings} {
    }

    FileResultSorter::FileResultSorter(const std::unique_ptr<FindSettings>& settings_ptr) : m_settings{*settings_ptr} {
    }

    FindSettings FileResultSorter::settings() const {
        return m_settings;
    }

    int cmp_file_results_by_path(const FileResult& fr1, const FileResult& fr2) {
        if (const int path_cmp = fr1.file_path().parent_path().compare(fr2.file_path().parent_path()); path_cmp != 0) {
            return path_cmp;
        }
        return fr1.file_path().filename().compare(fr2.file_path().filename());
    }

    int cmp_file_results_by_path_ci(const FileResult& fr1, const FileResult& fr2) {
        if (const int path_cmp = strcasecmp(fr1.file_path().parent_path().c_str(), fr2.file_path().parent_path().c_str()); path_cmp != 0) {
            return path_cmp;
        }
        return strcasecmp(fr1.file_path().filename().c_str(), fr2.file_path().filename().c_str());
    }

    std::function<bool(FileResult&, FileResult&)> FileResultSorter::get_cmp_file_results_by_path() const {
        if (m_settings.sort_descending()) {
            if (m_settings.sort_case_insensitive()) {
                return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_path_ci(fr2, fr1) <= 0; };
            }
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_path(fr2, fr1) <= 0; };
        }
        if (m_settings.sort_case_insensitive()) {
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_path_ci(fr1, fr2) <= 0; };
        }
        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_path(fr1, fr2) <= 0; };
    }

    int cmp_file_results_by_name(const FileResult& fr1, const FileResult& fr2) {
        if (const int file_cmp = fr1.file_path().filename().compare(fr2.file_path().filename()); file_cmp != 0) {
            return file_cmp;
        }
        return fr1.file_path().parent_path().compare(fr2.file_path().parent_path());
    }

    int cmp_file_results_by_name_ci(const FileResult& fr1, const FileResult& fr2) {
        if (const int file_cmp = strcasecmp(fr1.file_path().filename().c_str(), fr2.file_path().filename().c_str()); file_cmp != 0) {
            return file_cmp < 0;
        }
        return strcasecmp(fr1.file_path().parent_path().c_str(), fr2.file_path().parent_path().c_str()) < 0;
    }

    std::function<bool(FileResult&, FileResult&)> FileResultSorter::get_cmp_file_results_by_name() const {
        if (m_settings.sort_descending()) {
            if (m_settings.sort_case_insensitive()) {
                return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_name_ci(fr2, fr1) <= 0; };
            }
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_name(fr2, fr1) <= 0; };
        }
        if (m_settings.sort_case_insensitive()) {
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_name_ci(fr1, fr2) <= 0; };
        }
        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_name(fr1, fr2) <= 0; };
    }

    int cmp_file_results_by_size(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_size() == fr2.file_size()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return fr1.file_size() < fr2.file_size() ? -1 : 1;
    }

    int cmp_file_results_by_size_ci(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_size() == fr2.file_size()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return fr1.file_size() < fr2.file_size() ? -1 : 1;
    }

    std::function<bool(FileResult&, FileResult&)> FileResultSorter::get_cmp_file_results_by_size() const {
        if (m_settings.sort_descending()) {
            if (m_settings.sort_case_insensitive()) {
                return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_size_ci(fr2, fr1) <= 0; };
            }
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_size(fr2, fr1) <= 0; };
        }
        if (m_settings.sort_case_insensitive()) {
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_size_ci(fr1, fr2) <= 0; };
        }
        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_size(fr1, fr2) <= 0; };
    }

    int cmp_file_results_by_type(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_type() == fr2.file_type()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return fr1.file_type() < fr2.file_type() ? -1 : 1;
    }

    int cmp_file_results_by_type_ci(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_type() == fr2.file_type()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return fr1.file_type() < fr2.file_type() ? -1 : 1;
    }

    std::function<bool(FileResult&, FileResult&)> FileResultSorter::get_cmp_file_results_by_type() const {
        if (m_settings.sort_descending()) {
            if (m_settings.sort_case_insensitive()) {
                return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_type_ci(fr2, fr1) <= 0; };
            }
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_type(fr2, fr1) <= 0; };
        }
        if (m_settings.sort_case_insensitive()) {
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_type_ci(fr1, fr2) <= 0; };
        }
        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_type(fr1, fr2) <= 0; };
    }

    int cmp_file_results_by_lastmod(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.last_mod() == fr2.last_mod()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return fr1.last_mod() < fr2.last_mod() ? -1 : 1;
    }

    int cmp_file_results_by_lastmod_ci(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.last_mod() == fr2.last_mod()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return fr1.last_mod() < fr2.last_mod() ? -1 : 1;
    }

    std::function<bool(FileResult&, FileResult&)> FileResultSorter::get_cmp_file_results_by_lastmod() const {
        if (m_settings.sort_descending()) {
            if (m_settings.sort_case_insensitive()) {
                return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_lastmod_ci(fr2, fr1) <= 0; };
            }
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_lastmod(fr2, fr1) <= 0; };
        }
        if (m_settings.sort_case_insensitive()) {
            return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_lastmod_ci(fr1, fr2) <= 0; };
        }
        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_lastmod(fr1, fr2) <= 0; };
    }

    std::function<bool(FileResult&, FileResult&)> FileResultSorter::get_file_result_comparator() const {
        switch (m_settings.sort_by()) {
            case SortBy::FILENAME:
                return get_cmp_file_results_by_name();
            case SortBy::FILEPATH:
                return get_cmp_file_results_by_path();
            case SortBy::FILESIZE:
                return get_cmp_file_results_by_size();
            case SortBy::FILETYPE:
                return get_cmp_file_results_by_type();
            case SortBy::LASTMOD:
                return get_cmp_file_results_by_lastmod();
        }
        return get_cmp_file_results_by_path();
    }

    void FileResultSorter::sort(std::vector<FileResult>& file_results) const {
        const auto sort_comparator = get_file_result_comparator();
        std::ranges::sort(file_results, sort_comparator);
    }
}
