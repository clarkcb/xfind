#include <algorithm>
#include <sys/stat.h>
#include "common.h"
#include "FileUtil.h"
#include "FindException.h"
#include "StringUtil.h"
#include "Finder.h"

#include <iostream>
#include <unistd.h>
#include <utility>

#include "FileResultFormatter.h"
#include "FileResultSorter.h"

namespace cppfind {
    static bool matches_any_pattern(const std::string_view s,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        const std::string ss{s};
        return std::ranges::any_of(patterns.cbegin(), patterns.cend(), [ss](const RegexPattern& p) {
            return regex_search(ss, p.regex());
        });
    }

    static bool any_matches_any_pattern(const std::vector<std::string>& ss,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        return std::ranges::any_of(ss.cbegin(), ss.cend(), [patterns](const std::string& s) {
            return matches_any_pattern(s, patterns);
        });
    }

    static bool empty_or_matches_any_pattern(const std::string_view s,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        if (patterns.empty()) {
            return true;
        }
        const std::string ss{s};
        return std::ranges::any_of(patterns.cbegin(), patterns.cend(), [ss](const RegexPattern& p) {
            return regex_search(ss, p.regex());
        });
    }

    static bool empty_or_not_matches_any_pattern(const std::string_view s,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        if (patterns.empty()) {
            return true;
        }
        const std::string ss{s};
        return !std::ranges::any_of(patterns.cbegin(), patterns.cend(), [ss](const RegexPattern& p) {
            return regex_search(ss, p.regex());
        });
    }

    static bool empty_or_matches_any_string(const std::string_view s,
        const std::unordered_set<std::string>& string_set) {
        if (string_set.empty()) {
            return true;
        }
        const std::string str{s};
        return string_set.contains(str);
    }

    static bool empty_or_not_matches_any_string(const std::string_view s,
        const std::unordered_set<std::string>& string_set) {
        if (string_set.empty()) {
            return true;
        }
        const std::string str{s};
        return !string_set.contains(str);
    }

    static bool empty_or_matches_any_file_type(const FileType& file_type,
        const std::unordered_set<FileType>& file_types) {
        if (file_types.empty()) {
            return true;
        }
        return file_types.contains(file_type);
    }

    static bool empty_or_not_matches_any_file_type(const FileType& file_type,
        const std::unordered_set<FileType>& file_types) {
        if (file_types.empty()) {
            return true;
        }
        return !file_types.contains(file_type);
    }

    static bool path_matches_any_pattern(const std::filesystem::path& path,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        const std::vector<std::string> segments(path.begin(), path.end());
        return any_matches_any_pattern(segments, patterns);
    }

    static bool path_not_matches_any_pattern(const std::filesystem::path& path,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        const std::vector<std::string> segments(path.begin(), path.end());
        return !any_matches_any_pattern(segments, patterns);
    }

    static bool empty_or_path_matches_any_pattern(const std::filesystem::path& path,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        if (patterns.empty()) {
            return true;
        }
        return path_matches_any_pattern(path, patterns);
    }

    static bool empty_or_path_not_matches_any_pattern(const std::filesystem::path& path,
        const std::unordered_set<RegexPattern, RegexPatternHash>& patterns) {
        if (patterns.empty()) {
            return true;
        }
        return path_not_matches_any_pattern(path, patterns);
    }

    static bool is_matching_path_by_symlink(const std::filesystem::path& path, bool follow_symlinks) {
        return follow_symlinks || !std::filesystem::is_symlink(path);
    }

    static bool is_matching_path_by_hidden(const std::filesystem::path& path, bool include_hidden) {
        return include_hidden || !FileUtil::is_hidden_path(path);
    }

    static bool is_matching_file_name_by_hidden(const std::string& file_name, bool include_hidden) {
        return include_hidden || !FileUtil::is_hidden(file_name);
    }

    Finder::Finder(const FindSettings& settings) : m_settings{settings} {
        validate_settings(settings, m_file_types);
    }

    Finder::Finder(const std::unique_ptr<FindSettings>& settings_ptr) : m_settings{*settings_ptr} {
        validate_settings(m_settings, m_file_types);
    }

    void Finder::validate_settings(const FindSettings& settings, const FileTypes& file_types) {
        if (settings.paths().empty()) {
            throw FindException(STARTPATH_NOT_DEFINED);
        }
        for (const auto& path : settings.paths()) {
            auto p = path;
            if (!std::filesystem::exists(p)) {
                p = FileUtil::expand_path(p);
                if (!std::filesystem::exists(p)) {
                    throw FindException(STARTPATH_NOT_FOUND);
                }
            }
            if (access(p.c_str(), R_OK) != 0) {
                throw FindException(STARTPATH_NOT_READABLE);
            }

            // TODO: maybe we want to defer "matching" until the `find` phase?
            if (std::filesystem::is_symlink(p)) {
                if (!settings.follow_symlinks()) {
                    throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
                }
            } else if (std::filesystem::is_directory(p)) {
                if (!is_matching_path_by_hidden(p, settings.include_hidden())
                    || !empty_or_path_not_matches_any_pattern(p, settings.out_dir_patterns())) {
                    throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
                }
            } else if (std::filesystem::is_regular_file(p)) {
                if (const auto parent = p.parent_path();
                    !is_matching_path_by_hidden(parent, settings.include_hidden())
                    || !empty_or_path_matches_any_pattern(parent, settings.in_dir_patterns())
                    || !empty_or_path_not_matches_any_pattern(parent, settings.out_dir_patterns())
                    ) {
                    throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
                }

                const auto file_name = p.filename().string();
                const auto ext = FileUtil::get_path_extension(p.filename());

                if (!is_matching_file_name_by_hidden(file_name, settings.include_hidden())
                    || !empty_or_matches_any_string(file_name, settings.in_extensions())
                    || !empty_or_not_matches_any_string(file_name, settings.out_extensions())
                    || !empty_or_matches_any_pattern(file_name, settings.in_file_patterns())
                    || !empty_or_not_matches_any_pattern(file_name, settings.out_file_patterns())
                    ) {
                    throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
                }

                if (const auto file_type = file_types.get_path_type(p.filename());
                    !empty_or_matches_any_file_type(file_type, settings.in_file_types())
                    || !empty_or_not_matches_any_file_type(file_type, settings.out_file_types())) {
                    throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
                }

            } else {
                throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
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

    bool Finder::is_matching_dir_path_by_hidden(const std::filesystem::path& dir_path) const {
        return is_matching_path_by_hidden(dir_path, m_settings.include_hidden());
    }

    bool Finder::is_matching_dir_path_by_in_patterns(const std::filesystem::path& dir_path) const {
        return empty_or_path_matches_any_pattern(dir_path, m_settings.in_dir_patterns());
    }

    bool Finder::is_matching_dir_path_by_out_patterns(const std::filesystem::path& dir_path) const {
        return empty_or_path_not_matches_any_pattern(dir_path, m_settings.out_dir_patterns());
    }

    bool Finder::is_traversable_dir_path(const std::filesystem::path& dir_path) const {
        return is_matching_path_by_symlink(dir_path, m_settings.follow_symlinks())
            && is_matching_dir_path_by_hidden(dir_path)
            && is_matching_dir_path_by_out_patterns(dir_path);
    }

    bool Finder::is_matching_dir_path(const std::filesystem::path& dir_path) const {
        return is_matching_path_by_symlink(dir_path, m_settings.follow_symlinks())
            && is_matching_dir_path_by_hidden(dir_path)
            && is_matching_dir_path_by_in_patterns(dir_path)
            && is_matching_dir_path_by_out_patterns(dir_path);
    }

    bool Finder::is_empty_or_matching_dir_path(const std::filesystem::path& dir_path) const {
        return dir_path.empty() || is_matching_dir_path(dir_path);
    }

    bool Finder::is_matching_archive_extension(const std::string& file_ext) const {
        return empty_or_matches_any_string(file_ext, m_settings.in_archive_extensions())
            && empty_or_not_matches_any_string(file_ext, m_settings.out_archive_extensions());
    }

    bool Finder::has_matching_archive_extension(const std::filesystem::path& file_path) const {
        if (!m_settings.in_archive_extensions().empty() || !m_settings.out_archive_extensions().empty()) {
            const auto ext = FileUtil::get_path_extension(file_path);
            return is_matching_archive_extension(ext);
        }
        return true;
    }

    bool Finder::is_matching_archive_file_name(const std::string& file_name) const {
        return empty_or_matches_any_pattern(file_name, m_settings.in_archive_file_patterns())
            && empty_or_not_matches_any_pattern(file_name, m_settings.out_archive_file_patterns());
    }

    bool Finder::has_matching_archive_file_name(const std::filesystem::path& file_path) const {
        if (!m_settings.in_archive_file_patterns().empty() || !m_settings.out_archive_file_patterns().empty()) {
            const auto file_name = file_path.filename().string();
            return is_matching_archive_file_name(file_name);
        }
        return true;
    }

    bool Finder::is_matching_archive_file_path(const std::filesystem::path& file_path) const {
        return is_empty_or_matching_dir_path(file_path.parent_path())
            && is_matching_file_name_by_hidden(file_path.filename(), m_settings.include_hidden())
            && has_matching_archive_extension(file_path)
            && has_matching_archive_file_name(file_path);
    }

    bool Finder::is_matching_archive_file_result(const FileResult& file_result) const {
        return is_matching_archive_file_path(file_result.file_path());
    }

    bool Finder::is_matching_extension(const std::string& file_ext) const {
        return empty_or_matches_any_string(file_ext, m_settings.in_extensions())
            && empty_or_not_matches_any_string(file_ext, m_settings.out_extensions());
    }

    bool Finder::has_matching_extension(const std::filesystem::path& file_path) const {
        if (!m_settings.in_extensions().empty() || !m_settings.out_extensions().empty()) {
            const auto ext = FileUtil::get_path_extension(file_path);
            return is_matching_extension(ext);
        }
        return true;
    }

    bool Finder::is_matching_file_name(const std::string& file_name) const {
        return empty_or_matches_any_pattern(file_name, m_settings.in_file_patterns())
            && empty_or_not_matches_any_pattern(file_name, m_settings.out_file_patterns());
    }

    bool Finder::has_matching_file_name(const std::filesystem::path& file_path) const {
        if (!m_settings.in_file_patterns().empty() || !m_settings.out_file_patterns().empty()) {
            const auto file_name = file_path.filename().string();
            return is_matching_file_name(file_name);
        }
        return true;
    }

    bool Finder::is_matching_file_path(const std::filesystem::path& file_path) const {
        return is_matching_path_by_symlink(file_path, m_settings.follow_symlinks())
            && is_empty_or_matching_dir_path(file_path.parent_path())
            && is_matching_file_name_by_hidden(file_path.filename(), m_settings.include_hidden())
            && has_matching_extension(file_path)
            && has_matching_file_name(file_path);
    }

    bool Finder::is_matching_file_type(const FileType& file_type) const {
        return empty_or_matches_any_file_type(file_type, m_settings.in_file_types())
            && empty_or_not_matches_any_file_type(file_type, m_settings.out_file_types());
    }

    bool Finder::is_matching_file_size(const uint64_t file_size) const {
        return (m_settings.max_size() <= 0 || file_size <= m_settings.max_size())
            && (m_settings.min_size() <= 0 || file_size >= m_settings.min_size());
    }

    bool Finder::is_matching_last_mod(const long last_mod) const {
        return (m_settings.max_last_mod() <= 0 || last_mod <= m_settings.max_last_mod())
            && (m_settings.min_last_mod() <= 0 || last_mod >= m_settings.min_last_mod());
    }

    bool Finder::is_matching_file_result(const FileResult& file_result) const {
        return is_matching_file_path(file_result.file_path())
            && is_matching_file_type(file_result.file_type())
            && is_matching_file_size(file_result.file_size())
            && is_matching_last_mod(file_result.last_mod());
    }

    std::pair<uint64_t, long> Finder::get_file_path_size_and_last_mod(const std::filesystem::path& file_path) const {
        uint64_t file_size = 0;
        long last_mod = 0;
        if (m_settings.need_size()) {
            // get file size
            file_size = static_cast<uint64_t>(std::filesystem::file_size(file_path));
        }
        if (m_settings.need_last_mod()) {
            // get last write time
            const std::filesystem::file_time_type last_write_time = std::filesystem::last_write_time(file_path);

            // Convert to time since epoch (duration)
            const auto time_since_epoch = last_write_time.time_since_epoch();

            // Convert to seconds (or nanoseconds, etc.) depending on the representation
            const auto seconds = std::chrono::duration_cast<std::chrono::seconds>(time_since_epoch).count();

            // Convert to long
            last_mod = static_cast<long>(seconds);
        }
        return std::make_pair(file_size, last_mod);
    }

    std::optional<FileResult> Finder::filter_archive_file_path_to_file_result(const std::filesystem::path& file_path,
        const FileType file_type) const {
        if (!m_settings.include_archives() && !m_settings.archives_only()) {
            return std::nullopt;
        }
        if (!is_matching_archive_file_path(file_path)) {
            return std::nullopt;
        }

        auto file_result = FileResult(file_path, file_type, 0L, 0L);
        return std::optional{file_result};
    }

    std::optional<FileResult> Finder::filter_regular_file_path_to_file_result(const std::filesystem::path& file_path,
        const FileType file_type) const {
        if (m_settings.archives_only()) {
            return std::nullopt;
        }

        if (!is_matching_file_path(file_path) || !is_matching_file_type(file_type)) {
            return std::nullopt;
        }

        const auto [file_size, last_mod] = get_file_path_size_and_last_mod(file_path);

        if (!is_matching_file_size(file_size) || !is_matching_last_mod(last_mod)) {
            return std::nullopt;
        }

        auto file_result = FileResult(file_path, file_type, file_size, last_mod);
        return std::optional{file_result};
    }

    std::optional<FileResult> Finder::filter_to_file_result(const std::filesystem::path& file_path) const {
        if (!is_empty_or_matching_dir_path(file_path.parent_path())) {
            return std::nullopt;
        }

        if (!is_matching_path_by_hidden(file_path.filename(), m_settings.include_hidden())) {
            return std::nullopt;
        }

        const auto file_type = m_file_types.get_path_type(file_path.filename());
        if (file_type == FileType::ARCHIVE) {
            return filter_archive_file_path_to_file_result(file_path, file_type);
        }
        return filter_regular_file_path_to_file_result(file_path, file_type);
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
                if (entry.is_symlink()) {
                    if (!m_settings.follow_symlinks()) {
                        continue;
                    }
                }
                if (const std::filesystem::path& entry_path{entry.path()};
                    std::filesystem::is_directory(entry_path) && recurse
                    && is_traversable_dir_path(entry_path)) {
                    path_dirs.push_back(entry_path);
                } else if (std::filesystem::is_regular_file(entry_path) && (min_depth < 0 || current_depth >= min_depth)) {
                    if (auto opt_file_result = filter_to_file_result(entry_path);
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

    std::vector<FileResult> Finder::get_file_results(const std::filesystem::path& path) const {
        std::vector<FileResult> file_results{};

        std::filesystem::path p = path;
        if (!std::filesystem::exists(p)) {
            p = FileUtil::expand_path(path);
            if (!std::filesystem::exists(p)) {
                throw FindException(STARTPATH_NOT_FOUND);
            }
        }

        if (std::filesystem::is_symlink(p)) {
            if (!m_settings.follow_symlinks()) {
                throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
            }
        }
        if (is_directory(p)) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (m_settings.max_depth() == 0) {
                return file_results;
            }
            if (is_traversable_dir_path(p)) {
                const int max_depth = m_settings.recursive() ?  m_settings.max_depth() : 1;
                return rec_get_file_results(p, m_settings.min_depth(), max_depth, 1);
            }
            throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
        }
        if (std::filesystem::is_regular_file(p)) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (m_settings.min_depth() > 0) {
                return file_results;
            }
            if (auto opt_file_result = filter_to_file_result(p);
                opt_file_result.has_value()) {
                file_results.push_back(std::move(opt_file_result.value()));
            } else {
                throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
            }
        } else {
            throw FindException(STARTPATH_NOT_MATCH_FIND_SETTINGS);
        }

        return file_results;
    }

    std::vector<FileResult> Finder::find() const {
        std::vector<FileResult> file_results{};

        for (const auto& p : m_settings.paths()) {
            std::vector<FileResult> p_files = get_file_results(p);
            file_results.insert(file_results.end(), p_files.begin(), p_files.end());
        }

        const auto file_result_sorter = FileResultSorter(m_settings);
        file_result_sorter.sort(file_results);
        return file_results;
    }

    std::vector<std::filesystem::path> get_matching_dir_paths(const std::vector<FileResult>& file_results) {
        std::unordered_set<std::string> dir_set;
        std::vector<std::filesystem::path> matching_dir_paths;
        for (const auto& fr : file_results) {
            const std::string dir = fr.file_path().parent_path().string();
            if (!dir_set.contains(dir)) {
                matching_dir_paths.push_back(fr.file_path().parent_path());
            }
            dir_set.emplace(dir);
        }
        return matching_dir_paths;
    }

    void print_file_result_dirs(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter) {
        const std::vector<std::filesystem::path> dir_paths = get_matching_dir_paths(file_results);
        std::string msg{"\nMatching directories"};
        if (dir_paths.empty()) {
            msg.append(": 0");
            log_msg(msg);
        } else {
            msg.append(" (").append(std::to_string(dir_paths.size())).append("):");
            log_msg(msg);
            for (const auto& d : dir_paths) {
                log_msg(formatter.format_dir_path(d));
            }
        }
    }

    void print_file_results(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter) {
        std::string msg{"\nMatching files"};
        if (file_results.empty()) {
            msg.append(": 0");
            log_msg(msg);
        } else {
            msg.append(" (").append(std::to_string(file_results.size())).append("):");
            log_msg(msg);
            for (const auto& fr : file_results) {
                log_msg(formatter.format_file_result(fr));
            }
        }
    }
}
