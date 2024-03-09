#include <algorithm>
#include <sys/stat.h>
#include "FileUtil.h"
#include "FindException.h"
#include "StringUtil.h"
#include "Finder.h"

namespace cppfind {
    Finder::Finder(const FindSettings& settings) {
        validate_settings(settings);
        m_settings = settings;
        m_file_types = FileTypes();
    }

    void Finder::validate_settings(const FindSettings& settings) {
        if (settings.paths().empty()) {
            throw FindException("Startpath not defined");
        }
        for (const auto& p : settings.paths()) {
            if (!FileUtil::path_exists(p)) {
                throw FindException("Startpath not found");
            }
        }
        if (settings.max_depth() > -1 && settings.max_depth() < settings.min_depth()) {
            throw FindException("Invalid range for mindepth and maxdepth");
        }
        if (settings.max_last_mod() > 0 && settings.max_last_mod() < settings.min_last_mod()) {
            throw FindException("Invalid range for minlastmod and maxlastmod");
        }
        if (settings.max_size() > 0 && settings.max_size() < settings.min_size()) {
            throw FindException("Invalid range for minsize and maxsize");
        }
    }

    bool matches_any_pattern(const std::string_view s, const std::set<RegexPattern, RegexPatternCmp>& patterns) {
        const std::string ss{s};
        return std::ranges::any_of(patterns.cbegin(), patterns.cend(), [ss](const RegexPattern& p) {
            return regex_search(ss, p.regex());
        });
    }

    bool any_matches_any_pattern(const std::vector<std::string>& ss, const std::set<RegexPattern, RegexPatternCmp>& patterns) {
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

    bool Finder::is_matching_archive_file_result(const FileResult& file_result) const {
        if (!m_settings.in_archive_extensions().empty() || !m_settings.out_archive_extensions().empty()) {
            const auto ext = FileUtil::get_path_extension(file_result.file_path());
            if ((!m_settings.in_archive_extensions().empty() && !m_settings.in_archive_extensions().contains(ext)) ||
                 (!m_settings.out_archive_extensions().empty() && m_settings.out_archive_extensions().contains(ext))) {
                return false;
            }
        }
        if (!m_settings.in_archive_file_patterns().empty() || !m_settings.out_archive_file_patterns().empty()) {
            const auto file_name = file_result.file_path().filename().string();
            if ((!m_settings.in_archive_file_patterns().empty() &&
                !matches_any_pattern(file_name, m_settings.in_archive_file_patterns())) ||
                (!m_settings.out_archive_file_patterns().empty() &&
                    matches_any_pattern(file_name, m_settings.out_archive_file_patterns()))) {
                return false;
            }
        }
        return true;
    }

    bool Finder::is_matching_file_type(const FileType& file_type) const {
        if ((!m_settings.in_file_types().empty() && !m_settings.in_file_types().contains(file_type))
            || (!m_settings.out_file_types().empty() && m_settings.out_file_types().contains(file_type))) {
            return false;
        }
        return true;
    }

    bool Finder::is_matching_file_result(const FileResult& file_result) const {
        if (!m_settings.in_extensions().empty() || !m_settings.out_extensions().empty()) {
            const auto ext = FileUtil::get_path_extension(file_result.file_path());
            if ((!m_settings.in_extensions().empty() && !m_settings.in_extensions().contains(ext)) ||
                (!m_settings.out_extensions().empty() && m_settings.out_extensions().contains(ext))) {
                return false;
            }
        }
        if (!m_settings.in_file_patterns().empty() || !m_settings.out_file_patterns().empty()) {
            const auto file_name = file_result.file_path().filename().string();
            if ((!m_settings.in_file_patterns().empty() &&
                !matches_any_pattern(file_name, m_settings.in_file_patterns())) ||
                (!m_settings.out_file_patterns().empty() &&
                    matches_any_pattern(file_name, m_settings.out_file_patterns()))) {
                return false;
            }
        }
        if (!is_matching_file_type(file_result.file_type())) {
            return false;
        }
        if ((m_settings.max_last_mod() > 0 && file_result.mod_time() > m_settings.max_last_mod())
            || (m_settings.min_last_mod() > 0 && file_result.mod_time() < m_settings.min_last_mod())
            || (m_settings.max_size() > 0 && file_result.file_size() > m_settings.max_size())
            || (m_settings.min_size() > 0 && file_result.file_size() < m_settings.min_size())) {
            return false;
        }
        return true;
    }

    std::optional<FileResult> Finder::filter_to_file_result(std::filesystem::path&& file_path) const {
        if (!m_settings.include_hidden() && FileUtil::is_hidden_path(file_path.filename())) {
            return std::nullopt;
        }
        const auto file_type = m_file_types.get_path_type(file_path.filename());
        struct stat fpstat;
        uint64_t file_size = 0;
        long mod_time = 0;
        if (m_settings.need_stat()) {
            if (stat(file_path.c_str(), &fpstat) == -1) {
                // TODO: report error
                return std::nullopt;
            }
            file_size = static_cast<uint64_t>(fpstat.st_size);
            mod_time = static_cast<long>(fpstat.st_mtime);
        }
        auto file_result = FileResult(std::move(file_path), file_type, file_size, mod_time);
        if (file_type == FileType::ARCHIVE) {
            if (m_settings.include_archives() && is_matching_archive_file_result(file_result)) {
                return std::optional{file_result};
            }
            return std::nullopt;
        }
        if (!m_settings.archives_only() && is_matching_file_result(file_result)) {
            return std::optional{file_result};
        }
        return std::nullopt;
    }

    std::vector<FileResult> Finder::get_file_results(const std::filesystem::path& file_path, const int depth) {
        std::vector<FileResult> file_results{};

        std::vector<std::filesystem::directory_entry> dir_entries;
        copy(std::filesystem::directory_iterator(file_path), std::filesystem::directory_iterator(),
             back_inserter(dir_entries));

        std::vector<std::filesystem::path> matching_dirs{};

        for (const auto& de : dir_entries) {
            if (std::filesystem::path dir_path = de.path(); std::filesystem::is_directory(dir_path)
                && (m_settings.max_depth() < 1 || depth <= m_settings.max_depth())
                && m_settings.recursive()
                && is_matching_dir_path(dir_path.filename())) {
                matching_dirs.push_back(dir_path);
            } else if (std::filesystem::is_regular_file(dir_path)
                       && depth >= m_settings.min_depth()
                       && (m_settings.max_depth() < 1 || depth <= m_settings.max_depth())) {
                if (auto opt_file_result = filter_to_file_result(std::move(dir_path));
                    opt_file_result.has_value()) {
                    file_results.push_back(std::move(opt_file_result.value()));
                }
            }
        }

        for (const auto& matching_dir : matching_dirs) {
            std::vector<FileResult> sub_file_results = get_file_results(matching_dir, depth + 1);
            file_results.insert(file_results.end(), sub_file_results.begin(), sub_file_results.end());
        }

        return file_results;
    }

    std::vector<FileResult> Finder::find() {
        std::vector<FileResult> file_results{};

        for (const auto& p : m_settings.paths()) {
            // we check using expanded in case p has tilde
            if (auto expanded = FileUtil::expand_tilde(p); std::filesystem::is_directory(expanded)) {
                // if max_depth is zero, we can skip since a directory cannot be a result
                if (m_settings.max_depth() != 0) {
                    std::vector<FileResult> p_files = get_file_results(expanded, 1);
                    file_results.insert(file_results.end(), p_files.begin(), p_files.end());
                }

            } else if (std::filesystem::is_regular_file(expanded)) {
                // if min_depth > zero, we can skip since the file is at depth zero
                if (m_settings.min_depth() <= 0) {
                    if (auto opt_file_result = filter_to_file_result(std::move(expanded));
                        opt_file_result.has_value()) {
                        file_results.push_back(std::move(opt_file_result.value()));
                    }
                }

            } else {
                throw FindException("path is an unsupported file type");
            }
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
        const int pathcmp = strcasecmp(fr1.file_path().parent_path().c_str(), fr2.file_path().parent_path().c_str());
        if (pathcmp == 0) {
            return strcasecmp(fr1.file_path().filename().c_str(), fr2.file_path().filename().c_str()) < 0;
        }
        return pathcmp < 0;
    }

    std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_path(const FindSettings& settings) {
        if (settings.sort_case_insensitive()) {
            return cmp_file_results_by_path_ci;
        }
        return cmp_file_results_by_path;
    }

    bool cmp_file_results_by_name(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.file_path().filename() == fr2.file_path().filename()) {
            return (fr1.file_path().parent_path() < fr2.file_path().parent_path());
        }
        return (fr1.file_path().filename() < fr2.file_path().filename());
    }

    bool cmp_file_results_by_name_ci(const FileResult& fr1, const FileResult& fr2) {
        const int filecmp = strcasecmp(fr1.file_path().filename().c_str(), fr2.file_path().filename().c_str());
        if (filecmp == 0) {
            return strcasecmp(fr1.file_path().parent_path().c_str(), fr2.file_path().parent_path().c_str()) < 0;
        }
        return filecmp < 0;
    }

    std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_name(const FindSettings& settings) {
        if (settings.sort_case_insensitive()) {
            return cmp_file_results_by_name_ci;
        }
        return cmp_file_results_by_name;
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

    std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_size(const FindSettings& settings) {
        if (settings.sort_case_insensitive()) {
            return cmp_file_results_by_size_ci;
        }
        return cmp_file_results_by_size;
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

    std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_type(const FindSettings& settings) {
        if (settings.sort_case_insensitive()) {
            return cmp_file_results_by_type_ci;
        }
        return cmp_file_results_by_type;
    }

    bool cmp_file_results_by_lastmod(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.mod_time() == fr2.mod_time()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1.mod_time() < fr2.mod_time());
    }

    bool cmp_file_results_by_lastmod_ci(const FileResult& fr1, const FileResult& fr2) {
        if (fr1.mod_time() == fr2.mod_time()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1.mod_time() < fr2.mod_time());
    }

    std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_lastmod(const FindSettings& settings) {
        if (settings.sort_case_insensitive()) {
            return cmp_file_results_by_lastmod_ci;
        }
        return cmp_file_results_by_lastmod;
    }

    void Finder::sort_file_results(std::vector<FileResult>& file_results) const {
        if (m_settings.sort_by() == SortBy::FILEPATH) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_path(m_settings));
        } else if (m_settings.sort_by() == SortBy::FILENAME) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_name(m_settings));
        } else if (m_settings.sort_by() == SortBy::FILESIZE) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_size(m_settings));
        } else if (m_settings.sort_by() == SortBy::FILETYPE) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_type(m_settings));
        } else if (m_settings.sort_by() == SortBy::LASTMOD) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_lastmod(m_settings));
        }
        if (m_settings.sort_descending()) {
            std::reverse(file_results.begin(), file_results.end());
        }
    }
}
