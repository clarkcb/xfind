#include <algorithm>
#include <sys/stat.h>
#include "FileUtil.h"
#include "FindException.h"
#include "StringUtil.h"
#include "Finder.h"

#include <iostream>
#include <unistd.h>

namespace cppfind {
    Finder::Finder(const FindSettings& settings) : m_settings{settings} {
        validate_settings(settings);
    }

    Finder::Finder(const std::unique_ptr<FindSettings>& settings_ptr) : m_settings{*settings_ptr} {
        validate_settings(m_settings);
    }

    void Finder::validate_settings(const FindSettings& settings) {
        if (settings.paths().empty()) {
            throw FindException(STARTPATH_NOT_DEFINED);
        }
        for (const auto& p : settings.paths()) {
            if (std::filesystem::exists(p)) {
                if (access(p.c_str(), R_OK) != 0) {
                    throw FindException(STARTPATH_NOT_READABLE);
                }
             } else {
                 const std::filesystem::path expanded = FileUtil::expand_path(p);
                 if (std::filesystem::exists(expanded)) {
                     if (access(expanded.c_str(), R_OK) != 0) {
                         throw FindException(STARTPATH_NOT_READABLE);
                     }
                 } else {
                     throw FindException(STARTPATH_NOT_FOUND);
                 }
             }
        }
        if (settings.max_depth() > -1 && settings.max_depth() < settings.min_depth()) {
            throw FindException(INVALID_RANGE_MINDEPTH_MAXDEPTH);
        }
        if (settings.max_last_mod() > 0 && settings.max_last_mod() < settings.min_last_mod()) {
            throw FindException(INVALID_RANGE_MINLASTMOD_MAXLASTMOD);
        }
        if (settings.max_size() > 0 && settings.max_size() < settings.min_size()) {
            throw FindException(INVALID_RANGE_MINSIZE_MAXSIZE);
        }
    }

    bool matches_any_pattern(const std::string_view s, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        const std::string ss{s};
        return std::ranges::any_of(patterns.cbegin(), patterns.cend(), [ss](const RegexPattern& p) {
            return regex_search(ss, p.regex());
        });
    }

    bool any_matches_any_pattern(const std::vector<std::string>& ss,
                                 const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        return std::ranges::any_of(ss.cbegin(), ss.cend(), [patterns](const std::string& s) {
            return matches_any_pattern(s, patterns);
        });
    }

    bool Finder::is_matching_dir_path(const std::filesystem::path& dir_path) const {
        for (auto it = dir_path.begin(); it != dir_path.end(); ++it) {
            if (!m_settings.include_hidden() && FileUtil::is_hidden(it->string())) {
                return false;
            }
            if ((!m_settings.in_dir_patterns().empty()
                && !matches_any_pattern(it->string(), m_settings.in_dir_patterns()))
                || (!m_settings.out_dir_patterns().empty()
                    && matches_any_pattern(it->string(), m_settings.out_dir_patterns()))) {
                return false;
            }
        }
        return true;
    }

    bool Finder::is_matching_archive_extension(const std::string& file_ext) const {
        return (m_settings.in_archive_extensions().empty()
            || m_settings.in_archive_extensions().contains(file_ext))
            && (m_settings.out_archive_extensions().empty()
            || !m_settings.out_archive_extensions().contains(file_ext));
    }

    bool Finder::is_matching_extension(const std::string& file_ext) const {
        return (m_settings.in_extensions().empty()
            || m_settings.in_extensions().contains(file_ext))
            && (m_settings.out_extensions().empty()
            || !m_settings.out_extensions().contains(file_ext));
    }

    bool Finder::has_matching_archive_extension(const FileResult& file_result) const {
        if (!m_settings.in_archive_extensions().empty() || !m_settings.out_archive_extensions().empty()) {
            const auto ext = FileUtil::get_path_extension(file_result.file_path());
            return is_matching_archive_extension(ext);
        }
        return true;
    }

    bool Finder::has_matching_extension(const FileResult& file_result) const {
        if (!m_settings.in_extensions().empty() || !m_settings.out_extensions().empty()) {
            const auto ext = FileUtil::get_path_extension(file_result.file_path());
            return is_matching_extension(ext);
        }
        return true;
    }

    bool Finder::is_matching_archive_file_name(const std::string& file_name) const {
        return (m_settings.in_archive_file_patterns().empty()
            || matches_any_pattern(file_name, m_settings.in_archive_file_patterns()))
            && (m_settings.out_archive_file_patterns().empty()
                || !matches_any_pattern(file_name, m_settings.out_archive_file_patterns()));
    }

    bool Finder::is_matching_file_name(const std::string& file_name) const {
        return (m_settings.in_file_patterns().empty()
            || matches_any_pattern(file_name, m_settings.in_file_patterns()))
            && (m_settings.out_file_patterns().empty()
                || !matches_any_pattern(file_name, m_settings.out_file_patterns()));
    }

    bool Finder::is_matching_file_type(const FileType& file_type) const {
        return (m_settings.in_file_types().empty()
            || m_settings.in_file_types().contains(file_type))
            && (m_settings.out_file_types().empty()
                || !m_settings.out_file_types().contains(file_type));
    }

    bool Finder::is_matching_file_size(const uint64_t file_size) const {
        return (m_settings.max_size() == 0
            || file_size <= m_settings.max_size())
            && (m_settings.min_size() == 0
                || file_size >= m_settings.min_size());
    }

    bool Finder::is_matching_last_mod(const long last_mod) const {
        return (m_settings.max_last_mod() == 0
            || last_mod <= m_settings.max_last_mod())
            && (m_settings.min_last_mod() == 0
                || last_mod >= m_settings.min_last_mod());
    }

    bool Finder::is_matching_archive_file_result(const FileResult& file_result) const {
        return has_matching_archive_extension(file_result)
            && is_matching_archive_file_name(file_result.file_name());
    }

    bool Finder::is_matching_file_result(const FileResult& file_result) const {
        return has_matching_extension(file_result)
            && is_matching_file_name(file_result.file_name())
            && is_matching_file_type(file_result.file_type())
            && is_matching_file_size(file_result.file_size())
            && is_matching_last_mod(file_result.last_mod());
    }

    std::optional<FileResult> Finder::filter_to_file_result(const std::filesystem::path& file_path) const {
        if (!m_settings.include_hidden() && FileUtil::is_hidden_path(file_path.filename())) {
            return std::nullopt;
        }
        const auto file_type = m_file_types.get_path_type(file_path.filename());
        if (file_type == FileType::ARCHIVE && !m_settings.include_archives() && !m_settings.archives_only()) {
            return std::nullopt;
        }
        uint64_t file_size = 0;
        long last_mod = 0;
        if (m_settings.need_stat()) {
            // get file size
            file_size = static_cast<uint64_t>(std::filesystem::file_size(file_path));

            // get last write time
            const std::filesystem::file_time_type last_write_time = std::filesystem::last_write_time(file_path);

            // Convert to time since epoch (duration)
            const auto time_since_epoch = last_write_time.time_since_epoch();

            // Convert to seconds (or nanoseconds, etc.) depending on the representation
            const auto seconds = std::chrono::duration_cast<std::chrono::seconds>(time_since_epoch).count();

            // Convert to long
            last_mod = static_cast<long>(seconds);
        }
        auto file_result = FileResult(file_path, file_type, file_size, last_mod);
        if (file_type == FileType::ARCHIVE) {
            if (is_matching_archive_file_result(file_result)) {
                return std::optional{file_result};
            }
            return std::nullopt;
        }
        if (!m_settings.archives_only() && is_matching_file_result(file_result)) {
            return std::optional{file_result};
        }
        return std::nullopt;
    }

    std::vector<FileResult> Finder::rec_get_file_results(const std::filesystem::path& dir_path, // NOLINT(*-no-recursion)
        const int min_depth, const int max_depth, const int current_depth) const {
        std::vector<FileResult> file_results{};
        bool recurse = true;
        if (current_depth == max_depth) {
            recurse = false;
        } else if (max_depth > -1 && current_depth > max_depth) {
            return file_results;
        }

        std::vector<std::filesystem::path> path_dirs{};

        try {
            for (const std::filesystem::directory_iterator it{dir_path}; const auto& entry : it) {
                std::filesystem::path entry_path{entry.path()};
                if (entry.is_symlink()) {
                    if (m_settings.follow_symlinks()) {
                        // Redefined entry_path as the symlink's target path
                        entry_path = std::filesystem::read_symlink(entry_path);
                    } else {
                        continue;
                    }
                }
                if (std::filesystem::is_directory(entry_path) && recurse && is_matching_dir_path(entry.path().filename())) {
                    path_dirs.push_back(entry.path());
                } else if (std::filesystem::is_regular_file(entry_path) && (min_depth < 0 || current_depth >= min_depth)) {
                    if (auto opt_file_result = filter_to_file_result(entry.path());
                        opt_file_result.has_value()) {
                        file_results.push_back(std::move(opt_file_result.value()));
                    }
                }
            }
        } catch (const std::filesystem::filesystem_error& e) {
            std::cerr << "Error: " << e.what() << std::endl;
        }

        for (const auto& path_dir : path_dirs) {
            std::vector<FileResult> path_dir_results = rec_get_file_results(path_dir, min_depth, max_depth,
                current_depth + 1);
            file_results.insert(file_results.end(), path_dir_results.begin(), path_dir_results.end());
        }

        return file_results;
    }

    std::vector<FileResult> Finder::get_file_results(const std::filesystem::path& file_path) const {
        std::vector<FileResult> file_results{};

        std::filesystem::path fp = file_path;
        if (!std::filesystem::exists(fp)) {
            fp = FileUtil::expand_path(file_path);
        }

        if (is_directory(fp)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (m_settings.max_depth() == 0) {
                return file_results;
            }
            if (is_matching_dir_path(fp)) {
                const int max_depth = m_settings.recursive() ?  m_settings.max_depth() : 1;
                return rec_get_file_results(fp, m_settings.min_depth(), max_depth, 1);
            }
        } else {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (m_settings.min_depth() > 0) {
                return file_results;
            }
            if (auto opt_file_result = filter_to_file_result(fp);
                opt_file_result.has_value()) {
                file_results.push_back(std::move(opt_file_result.value()));
            }
        }

        return file_results;
    }

    std::vector<FileResult> Finder::find() const {
        std::vector<FileResult> file_results{};

        for (const auto& p : m_settings.paths()) {
            std::vector<FileResult> p_files = get_file_results(p);
            file_results.insert(file_results.end(), p_files.begin(), p_files.end());
        }

        sort_file_results(file_results);
        return file_results;
    }

    bool cmp_file_results_by_path(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_path().parent_path() == fr2.file_path().parent_path()) {
            return (fr1.file_path().filename().compare(fr2.file_path().filename()) < 0);
        }
        return (fr1.file_path().parent_path().compare(fr2.file_path().parent_path()) < 0);
    }

    bool cmp_file_results_by_path_ci(const FileResult& fr1, const FileResult& fr2) {
        const int path_cmp = strcasecmp(fr1.file_path().parent_path().c_str(), fr2.file_path().parent_path().c_str());
        if (path_cmp == 0) {
            return strcasecmp(fr1.file_path().filename().c_str(), fr2.file_path().filename().c_str()) < 0;
        }
        return path_cmp < 0;
    }

    bool cmp_file_results_by_name(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_path().filename() == fr2.file_path().filename()) {
            return (fr1.file_path().parent_path() < fr2.file_path().parent_path());
        }
        return (fr1.file_path().filename() < fr2.file_path().filename());
    }

    bool cmp_file_results_by_name_ci(const FileResult& fr1, const FileResult& fr2) {
        const int file_cmp = strcasecmp(fr1.file_path().filename().c_str(), fr2.file_path().filename().c_str());
        if (file_cmp == 0) {
            return strcasecmp(fr1.file_path().parent_path().c_str(), fr2.file_path().parent_path().c_str()) < 0;
        }
        return file_cmp < 0;
    }

    bool cmp_file_results_by_size(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_size() == fr2.file_size()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1.file_size() < fr2.file_size());
    }

    bool cmp_file_results_by_size_ci(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_type() == fr2.file_type()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1.file_type() < fr2.file_type());
    }

    bool cmp_file_results_by_type(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_type() == fr2.file_type()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1.file_type() < fr2.file_type());
    }

    bool cmp_file_results_by_type_ci(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_type() == fr2.file_type()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1.file_type() < fr2.file_type());
    }

    bool cmp_file_results_by_lastmod(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.last_mod() == fr2.last_mod()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1.last_mod() < fr2.last_mod());
    }

    bool cmp_file_results_by_lastmod_ci(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.last_mod() == fr2.last_mod()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1.last_mod() < fr2.last_mod());
    }

    std::function<bool(FileResult&, FileResult&)> get_sort_comparator(const FindSettings& settings) {
        if (settings.sort_descending()) {
            if (settings.sort_case_insensitive()) {
                switch (settings.sort_by()) {
                    case SortBy::FILENAME:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_name_ci(fr2, fr1); };
                    case SortBy::FILEPATH:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_path_ci(fr2, fr1); };
                    case SortBy::FILESIZE:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_size_ci(fr2, fr1); };
                    case SortBy::FILETYPE:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_type_ci(fr2, fr1); };
                    case SortBy::LASTMOD:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_lastmod_ci(fr2, fr1); };
                }
            } else {
                switch (settings.sort_by()) {
                    case SortBy::FILENAME:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_name(fr2, fr1); };
                    case SortBy::FILEPATH:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_path(fr2, fr1); };
                    case SortBy::FILESIZE:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_size(fr2, fr1); };
                    case SortBy::FILETYPE:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_type(fr2, fr1); };
                    case SortBy::LASTMOD:
                        return [](const FileResult& fr1, const FileResult& fr2) { return cmp_file_results_by_lastmod(fr2, fr1); };
                }
            }
        } else {
            if (settings.sort_case_insensitive()) {
                switch (settings.sort_by()) {
                    case SortBy::FILENAME:
                        return cmp_file_results_by_name_ci;
                    case SortBy::FILEPATH:
                        return cmp_file_results_by_path_ci;
                    case SortBy::FILESIZE:
                        return cmp_file_results_by_size_ci;
                    case SortBy::FILETYPE:
                        return cmp_file_results_by_type_ci;
                    case SortBy::LASTMOD:
                        return cmp_file_results_by_lastmod_ci;
                }
            } else {
                switch (settings.sort_by()) {
                    case SortBy::FILENAME:
                        return cmp_file_results_by_name;
                    case SortBy::FILEPATH:
                        return cmp_file_results_by_path;
                    case SortBy::FILESIZE:
                        return cmp_file_results_by_size;
                    case SortBy::FILETYPE:
                        return cmp_file_results_by_type;
                    case SortBy::LASTMOD:
                        return cmp_file_results_by_lastmod;
                }
            }
        }
        return cmp_file_results_by_lastmod;
    }

    void Finder::sort_file_results(std::vector<FileResult>& file_results) const {
        const auto sort_comparator = get_sort_comparator(m_settings);
        std::ranges::sort(file_results, sort_comparator);
    }
}
