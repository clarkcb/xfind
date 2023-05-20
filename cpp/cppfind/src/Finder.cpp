#include <algorithm>
#include <boost/filesystem.hpp>
#include <optional>
#include <sys/stat.h>

#include "FileTypes.h"
#include "FileUtil.h"
#include "FindException.h"
#include "Finder.h"

namespace cppfind {
    Finder::Finder(FindSettings* settings) {
        validate_settings(settings);
        m_settings = settings;
        m_file_types = new FileTypes();
    }

    void Finder::validate_settings(FindSettings* settings) {
        if (settings->paths()->empty()) {
            throw FindException("Startpath not defined");
        }
        for (auto& p : *settings->paths()) {
            if (!FileUtil::file_exists(p)) {
                std::string expanded = FileUtil::expand_path(p);
                if (!FileUtil::file_exists(expanded)) {
                    throw FindException("Startpath not found");
                }
            }
        }
    }

    FileResult* Finder::get_file_result(std::string& file_path) {
        boost::filesystem::path path(file_path);
        std::string parent_path = path.parent_path().string();
        std::string file_name = path.filename().string();
        FileType file_type = m_file_types->get_file_type(file_path);
        struct stat st;
        if (stat(file_path.c_str(), &st))
            return nullptr;
        auto file_size = (uint64_t) st.st_size;
        auto mod_time = (long) st.st_mtime;
        return new FileResult(parent_path, file_name, file_type, file_size, mod_time);
    }

    std::vector<FileResult*> Finder::find() {
        std::vector<FileResult*> file_results{};

        for (auto& p : *m_settings->paths()) {
            // we check using expanded in case p has tilde
            std::string expanded = FileUtil::expand_path(p);

            if (FileUtil::is_directory(expanded)) {
                std::vector<FileResult*> p_files = get_file_results(expanded);
                file_results.insert(file_results.end(), p_files.begin(), p_files.end());

            } else if (FileUtil::is_regular_file(expanded)) {
                auto* fr = get_file_result(expanded);
                file_results.push_back(fr);

            } else {
                throw FindException("path is an unsupported file type");
            }
        }

        sort_file_results(file_results);
        return file_results;
    }

    bool cmp_file_results_by_path(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->path() == fr2->path()) {
            return (fr1->file_name().compare(fr2->file_name()) < 0);
        }
        return (fr1->path().compare(fr2->path()) < 0);
    }

    bool cmp_file_results_by_path_ci(const FileResult* fr1, const FileResult* fr2) {
        int pathcmp = strcasecmp(fr1->path().c_str(), fr2->path().c_str());
        if (pathcmp == 0) {
            return strcasecmp(fr1->file_name().c_str(), fr2->file_name().c_str()) < 0;
        }
        return pathcmp < 0;
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_path(FindSettings* settings) {
        if (settings->sort_case_insensitive()) {
            return cmp_file_results_by_path_ci;
        } else {
            return cmp_file_results_by_path;
        }
    }

    bool cmp_file_results_by_name(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->file_name() == fr2->file_name()) {
            return (fr1->path() < fr2->path());
        }
        return (fr1->file_name() < fr2->file_name());
    }

    bool cmp_file_results_by_name_ci(const FileResult* fr1, const FileResult* fr2) {
        int filecmp = strcasecmp(fr1->file_name().c_str(), fr2->file_name().c_str());
        if (filecmp == 0) {
            return strcasecmp(fr1->path().c_str(), fr2->path().c_str()) < 0;
        }
        return filecmp < 0;
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_name(FindSettings* settings) {
        if (settings->sort_case_insensitive()) {
            return cmp_file_results_by_name_ci;
        } else {
            return cmp_file_results_by_name;
        }
    }

    bool cmp_file_results_by_size(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->file_size() == fr2->file_size()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1->file_size() < fr2->file_size());
    }

    bool cmp_file_results_by_size_ci(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->file_type() == fr2->file_type()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1->file_type() < fr2->file_type());
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_size(FindSettings* settings) {
        if (settings->sort_case_insensitive()) {
            return cmp_file_results_by_size_ci;
        } else {
            return cmp_file_results_by_size;
        }
    }

    bool cmp_file_results_by_type(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->file_type() == fr2->file_type()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1->file_type() < fr2->file_type());
    }

    bool cmp_file_results_by_type_ci(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->file_type() == fr2->file_type()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1->file_type() < fr2->file_type());
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_type(FindSettings* settings) {
        if (settings->sort_case_insensitive()) {
            return cmp_file_results_by_type_ci;
        } else {
            return cmp_file_results_by_type;
        }
    }

    bool cmp_file_results_by_lastmod(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->mod_time() == fr2->mod_time()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1->mod_time() < fr2->mod_time());
    }

    bool cmp_file_results_by_lastmod_ci(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->mod_time() == fr2->mod_time()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1->mod_time() < fr2->mod_time());
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_lastmod(FindSettings* settings) {
        if (settings->sort_case_insensitive()) {
            return cmp_file_results_by_lastmod_ci;
        } else {
            return cmp_file_results_by_lastmod;
        }
    }

    void Finder::sort_file_results(std::vector<FileResult*>& file_results) {
        if (m_settings->sort_by() == SortBy::FILEPATH) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_path(m_settings));
        } else if (m_settings->sort_by() == SortBy::FILENAME) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_name(m_settings));
        } else if (m_settings->sort_by() == SortBy::FILESIZE) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_size(m_settings));
        } else if (m_settings->sort_by() == SortBy::FILETYPE) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_type(m_settings));
        } else if (m_settings->sort_by() == SortBy::LASTMOD) {
            std::sort(file_results.begin(), file_results.end(), get_cmp_file_results_by_lastmod(m_settings));
        }
        if (m_settings->sort_descending()) {
            std::reverse(file_results.begin(), file_results.end());
        }
    }

    bool matches_any_pattern(const std::string& s, const std::vector<FindPattern*>& patterns) {
        std::smatch pmatch;
        for (auto& p : patterns) {
            if (regex_search(s, pmatch, p->r())) {
                return true;
            }
        }
        return false;
    }

    bool any_matches_any_pattern(const std::vector<std::string>& ss, const std::vector<FindPattern*>& patterns) {
        return std::any_of(ss.begin(), ss.end(), [patterns](const std::string& s) {
            return matches_any_pattern(s, patterns);
        });
    }

    bool Finder::is_matching_dir(const std::string& file_path) {
        std::vector<std::string> elems = FileUtil::split_path(file_path);
        if (m_settings->exclude_hidden()) {
            for (auto& elem : elems) {
                if (FileUtil::is_hidden(elem)) {
                    return false;
                }
            }
        }
        std::vector<FindPattern*>* in_dir_patterns = m_settings->in_dir_patterns();
        std::vector<FindPattern*>* out_dir_patterns = m_settings->out_dir_patterns();
        return ((in_dir_patterns->empty() || any_matches_any_pattern(elems, *in_dir_patterns))
                && (out_dir_patterns->empty() || !any_matches_any_pattern(elems, *out_dir_patterns)));
    }

    bool Finder::is_matching_archive_file(const std::string& file_name) {
        std::string ext = FileUtil::get_extension(file_name);
        std::vector <std::string>* in_exts = m_settings->in_archive_extensions();
        std::vector <std::string>* out_exts = m_settings->out_archive_extensions();
        if ((!in_exts->empty() && std::find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
            || (!out_exts->empty() && std::find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
            return false;
        }
        std::vector<FindPattern*>* in_file_patterns = m_settings->in_archive_file_patterns();
        std::vector<FindPattern*>* out_file_patterns = m_settings->out_archive_file_patterns();
        return ((in_file_patterns->empty() || matches_any_pattern(file_name, *in_file_patterns))
                && (out_file_patterns->empty() || !matches_any_pattern(file_name, *out_file_patterns)));
    }

    bool Finder::is_matching_file(const std::string& file_name, const FileType file_type, const struct stat* fpstat) {
        if (!m_settings->in_extensions()->empty() || !m_settings->out_extensions()->empty()) {
            std::string ext = FileUtil::get_extension(file_name);
            std::vector <std::string>* in_exts = m_settings->in_extensions();
            std::vector <std::string>* out_exts = m_settings->out_extensions();
            if ((!in_exts->empty() && std::find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
                || (!out_exts->empty() && std::find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
                return false;
            }
        }
        std::vector<FindPattern*>* in_file_patterns = m_settings->in_file_patterns();
        std::vector<FindPattern*>* out_file_patterns = m_settings->out_file_patterns();
        if ((!in_file_patterns->empty() && !matches_any_pattern(file_name, *in_file_patterns))
            || (!out_file_patterns->empty() && matches_any_pattern(file_name, *out_file_patterns))) {
            return false;
        }
        std::vector<FileType>* in_file_types = m_settings->in_file_types();
        std::vector<FileType>* out_file_types = m_settings->out_file_types();
        if ((!in_file_types->empty() && std::find(in_file_types->begin(), in_file_types->end(), file_type) == in_file_types->end())
            || (!out_file_types->empty() && std::find(out_file_types->begin(), out_file_types->end(), file_type) != out_file_types->end())) {
            return false;
        }
        if ((m_settings->max_last_mod() > 0 && fpstat->st_mtime > m_settings->max_last_mod())
            || (m_settings->min_last_mod() > 0 && fpstat->st_mtime < m_settings->min_last_mod())
            || (m_settings->max_size() > 0 && fpstat->st_size > m_settings->max_size())
               || (m_settings->min_size() > 0 && fpstat->st_size < m_settings->min_size())) {
            return false;
        }
        return true;
    }

    std::optional<FileResult*> Finder::filter_to_file_result(const std::string& file_path) {
        boost::filesystem::path p(file_path);
        std::string file_name = p.filename().string();
        if (m_settings->exclude_hidden() && FileUtil::is_hidden(file_name)) {
            return std::nullopt;
        }
        std::string parent_path = p.parent_path().string();
        auto file_type = m_file_types->get_file_type(file_name);
        struct stat fpstat;
        if (stat(file_path.c_str(), &fpstat) == -1) {
            // TODO: report error
            return std::nullopt;
        }
        auto file_size = (uint64_t) fpstat.st_size;
        auto mod_time = (long) fpstat.st_mtime;
        auto file_result = new FileResult(parent_path, file_name, file_type, file_size, mod_time);
        if (file_type == FileType::ARCHIVE) {
            if (m_settings->include_archives() && is_matching_archive_file(file_name)) {
                return std::optional<FileResult*>{file_result};
            }
            return std::nullopt;
        }
        if (!m_settings->archives_only() && is_matching_file(file_name, file_type, &fpstat)) {
            return std::optional<FileResult*>{file_result};
        }
        return std::nullopt;
    }

    std::vector<FileResult*> Finder::get_file_results(const std::string& file_path) {
        boost::filesystem::path p(file_path);
        std::vector<std::string> find_dirs{};
        std::vector<FileResult*> file_results{};

        std::vector<boost::filesystem::directory_entry> dir_entries;
        copy(boost::filesystem::directory_iterator(p), boost::filesystem::directory_iterator(),
             back_inserter(dir_entries));

        for (const auto& de : dir_entries) {
            const boost::filesystem::path& sub_path = de.path();
            if (boost::filesystem::is_directory(sub_path) && m_settings->recursive() && is_matching_dir(sub_path.string())) {
                find_dirs.push_back(sub_path.string());
            } else if (boost::filesystem::is_regular_file(sub_path)) {
                std::optional<FileResult*> optFileResult = filter_to_file_result(sub_path.string());
                if (optFileResult.has_value()) {
                    file_results.push_back(optFileResult.value());
                }
            }
        }

        for (const auto& find_dir : find_dirs) {
            std::vector<FileResult*> sub_file_results = get_file_results(find_dir);
            file_results.insert(file_results.end(), sub_file_results.begin(), sub_file_results.end());
        }

        return file_results;
    }
}
