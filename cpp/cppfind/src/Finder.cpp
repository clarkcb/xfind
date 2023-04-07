#include <algorithm>
#include <boost/filesystem.hpp>
#include <optional>
#include <sys/stat.h>

#include "FileTypes.h"
#include "FileUtil.h"
#include "FindException.h"
#include "Finder.h"

namespace cppfind {
    Finder::Finder(FindSettings* ss) {
        validate_settings(ss);
        m_settings = ss;
        m_filetypes = new FileTypes();
    }

    void Finder::validate_settings(FindSettings* ss) {
        if (ss->paths()->empty()) {
            throw FindException("Startpath not defined");
        }
        for (auto& p : *ss->paths()) {
            if (!FileUtil::file_exists(p)) {
                std::string expanded = FileUtil::expand_path(p);
                if (!FileUtil::file_exists(expanded)) {
                    throw FindException("Startpath not found");
                }
            }
        }
    }

    FileResult* Finder::get_file_result(std::string& filepath) {
        boost::filesystem::path path(filepath);
        std::string parent_path = path.parent_path().string();
        std::string filename = path.filename().string();
        FileType filetype = m_filetypes->get_filetype(filepath);
        struct stat st;
        if (stat(filepath.c_str(), &st))
            return nullptr;
        auto filesize = (uint64_t) st.st_size;
        auto modtime =  (long) st.st_mtime;
        return new FileResult(parent_path, filename, filetype, filesize, modtime);
    }

    std::vector<FileResult*> Finder::find() {
        std::vector<FileResult*> fileresults{};

        for (auto& p : *m_settings->paths()) {
            // we check using expanded in case p has tilde
            std::string expanded = FileUtil::expand_path(p);

            if (FileUtil::is_directory(expanded)) {
                std::vector<FileResult*> p_files = get_file_results(expanded);
                fileresults.insert(fileresults.end(), p_files.begin(), p_files.end());

            } else if (FileUtil::is_regular_file(expanded)) {
                auto* fr = get_file_result(expanded);
                fileresults.push_back(fr);

            } else {
                throw FindException("path is an unsupported file type");
            }
        }

        sort_file_results(fileresults);
        return fileresults;
    }

    bool cmp_file_results_by_path(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->path() == fr2->path()) {
            return (fr1->filename().compare(fr2->filename()) < 0);
        }
        return (fr1->path().compare(fr2->path()) < 0);
    }

    bool cmp_file_results_by_path_ci(const FileResult* fr1, const FileResult* fr2) {
        int pathcmp = strcasecmp(fr1->path().c_str(), fr2->path().c_str());
        if (pathcmp == 0) {
            return strcasecmp(fr1->filename().c_str(), fr2->filename().c_str()) < 0;
        }
        return pathcmp < 0;
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_path(FindSettings* settings) {
        if (settings->sort_caseinsensitive()) {
            return cmp_file_results_by_path_ci;
        } else {
            return cmp_file_results_by_path;
        }
    }

    bool cmp_file_results_by_name(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->filename() == fr2->filename()) {
            return (fr1->path() < fr2->path());
        }
        return (fr1->filename() < fr2->filename());
    }

    bool cmp_file_results_by_name_ci(const FileResult* fr1, const FileResult* fr2) {
        int filecmp = strcasecmp(fr1->filename().c_str(), fr2->filename().c_str());
        if (filecmp == 0) {
            return strcasecmp(fr1->path().c_str(), fr2->path().c_str()) < 0;
        }
        return filecmp < 0;
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_name(FindSettings* settings) {
        if (settings->sort_caseinsensitive()) {
            return cmp_file_results_by_name_ci;
        } else {
            return cmp_file_results_by_name;
        }
    }

    bool cmp_file_results_by_size(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->filesize() == fr2->filesize()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1->filesize() < fr2->filesize());
    }

    bool cmp_file_results_by_size_ci(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->filetype() == fr2->filetype()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1->filetype() < fr2->filetype());
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_size(FindSettings* settings) {
        if (settings->sort_caseinsensitive()) {
            return cmp_file_results_by_size_ci;
        } else {
            return cmp_file_results_by_size;
        }
    }

    bool cmp_file_results_by_type(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->filetype() == fr2->filetype()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1->filetype() < fr2->filetype());
    }

    bool cmp_file_results_by_type_ci(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->filetype() == fr2->filetype()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1->filetype() < fr2->filetype());
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_type(FindSettings* settings) {
        if (settings->sort_caseinsensitive()) {
            return cmp_file_results_by_type_ci;
        } else {
            return cmp_file_results_by_type;
        }
    }

    bool cmp_file_results_by_lastmod(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->modtime() == fr2->modtime()) {
            return cmp_file_results_by_path(fr1, fr2);
        }
        return (fr1->modtime() < fr2->modtime());
    }

    bool cmp_file_results_by_lastmod_ci(const FileResult* fr1, const FileResult* fr2) {
        if (fr1->modtime() == fr2->modtime()) {
            return cmp_file_results_by_path_ci(fr1, fr2);
        }
        return (fr1->modtime() < fr2->modtime());
    }

    std::function<bool(FileResult*, FileResult*)> get_cmp_file_results_by_lastmod(FindSettings* settings) {
        if (settings->sort_caseinsensitive()) {
            return cmp_file_results_by_lastmod_ci;
        } else {
            return cmp_file_results_by_lastmod;
        }
    }

    void Finder::sort_file_results(std::vector<FileResult*>& fileresults) {
        if (m_settings->sortby() == SortBy::FILEPATH) {
            std::sort(fileresults.begin(), fileresults.end(), get_cmp_file_results_by_path(m_settings));
        } else if (m_settings->sortby() == SortBy::FILENAME) {
            std::sort(fileresults.begin(), fileresults.end(), get_cmp_file_results_by_name(m_settings));
        } else if (m_settings->sortby() == SortBy::FILESIZE) {
            std::sort(fileresults.begin(), fileresults.end(), get_cmp_file_results_by_size(m_settings));
        } else if (m_settings->sortby() == SortBy::FILETYPE) {
            std::sort(fileresults.begin(), fileresults.end(), get_cmp_file_results_by_type(m_settings));
        } else if (m_settings->sortby() == SortBy::LASTMOD) {
            std::sort(fileresults.begin(), fileresults.end(), get_cmp_file_results_by_lastmod(m_settings));
        }
        if (m_settings->sort_descending()) {
            std::reverse(fileresults.begin(), fileresults.end());
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

    bool Finder::is_matching_dir(const std::string& filepath) {
        std::vector<std::string> elems = FileUtil::split_path(filepath);
        if (m_settings->excludehidden()) {
            for (auto& elem : elems) {
                if (FileUtil::is_hidden(elem)) {
                    return false;
                }
            }
        }
        std::vector<FindPattern*>* in_dirpatterns = m_settings->in_dirpatterns();
        std::vector<FindPattern*>* out_dirpatterns = m_settings->out_dirpatterns();
        return ((in_dirpatterns->empty() || any_matches_any_pattern(elems, *in_dirpatterns))
                && (out_dirpatterns->empty() || !any_matches_any_pattern(elems, *out_dirpatterns)));
    }

    bool Finder::is_matching_archive_file(const std::string& filename) {
        std::string ext = FileUtil::get_extension(filename);
        std::vector <std::string>* in_exts = m_settings->in_archiveextensions();
        std::vector <std::string>* out_exts = m_settings->out_archiveextensions();
        if ((!in_exts->empty() && std::find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
            || (!out_exts->empty() && std::find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
            return false;
        }
        std::vector<FindPattern*>* in_filepatterns = m_settings->in_archivefilepatterns();
        std::vector<FindPattern*>* out_filepatterns = m_settings->out_archivefilepatterns();
        return ((in_filepatterns->empty() || matches_any_pattern(filename, *in_filepatterns))
                && (out_filepatterns->empty() || !matches_any_pattern(filename, *out_filepatterns)));
    }

    bool Finder::is_matching_file(const std::string& filename, const FileType filetype, const struct stat* fpstat) {
        if (!m_settings->in_extensions()->empty() || !m_settings->out_extensions()->empty()) {
            std::string ext = FileUtil::get_extension(filename);
            std::vector <std::string>* in_exts = m_settings->in_extensions();
            std::vector <std::string>* out_exts = m_settings->out_extensions();
            if ((!in_exts->empty() && std::find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
                || (!out_exts->empty() && std::find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
                return false;
            }
        }
        std::vector<FindPattern*>* in_filepatterns = m_settings->in_filepatterns();
        std::vector<FindPattern*>* out_filepatterns = m_settings->out_filepatterns();
        if ((!in_filepatterns->empty() && !matches_any_pattern(filename, *in_filepatterns))
            || (!out_filepatterns->empty() && matches_any_pattern(filename, *out_filepatterns))) {
            return false;
        }
        std::vector<FileType>* in_filetypes = m_settings->in_filetypes();
        std::vector<FileType>* out_filetypes = m_settings->out_filetypes();
        if ((!in_filetypes->empty() && std::find(in_filetypes->begin(), in_filetypes->end(), filetype) == in_filetypes->end())
            || (!out_filetypes->empty() && std::find(out_filetypes->begin(), out_filetypes->end(), filetype) != out_filetypes->end())) {
            return false;
        }
        if ((m_settings->maxlastmod() > 0 && fpstat->st_mtime > m_settings->maxlastmod())
            || (m_settings->minlastmod() > 0 && fpstat->st_mtime < m_settings->minlastmod())
            || (m_settings->maxsize() > 0 && fpstat->st_size > m_settings->maxsize())
               || (m_settings->minsize() > 0 && fpstat->st_size < m_settings->minsize())) {
            return false;
        }
        return true;
    }

    bool Finder::filter_file(const std::string& filepath) {
        boost::filesystem::path p(filepath);
        std::string filename = p.filename().string();
        if (m_settings->excludehidden() && FileUtil::is_hidden(filename)) {
            return false;
        }
        auto filetype = m_filetypes->get_filetype(filename);
        struct stat fpstat;
        if (stat(filepath.c_str(), &fpstat) == -1) {
            // TODO: report error
            return false;
        }

        if (filetype == FileType::ARCHIVE) {
            return m_settings->includearchives() && is_matching_archive_file(filename);
        }
        return !m_settings->archivesonly() && is_matching_file(filename, filetype, &fpstat);
    }

    std::optional<FileResult*> Finder::filter_to_file_result(const std::string& filepath) {
        boost::filesystem::path p(filepath);
        std::string filename = p.filename().string();
        if (m_settings->excludehidden() && FileUtil::is_hidden(filename)) {
            return std::nullopt;
        }
        std::string parent_path = p.parent_path().string();
        auto filetype = m_filetypes->get_filetype(filename);
        struct stat fpstat;
        if (stat(filepath.c_str(), &fpstat) == -1) {
            // TODO: report error
            return std::nullopt;
        }
        auto filesize = (uint64_t) fpstat.st_size;
        auto modtime =  (long) fpstat.st_mtime;
        auto fileresult = new FileResult(parent_path, filename, filetype, filesize, modtime);
        if (filetype == FileType::ARCHIVE) {
            if (m_settings->includearchives() && is_matching_archive_file(filename)) {
                return std::optional<FileResult*>{fileresult};
            }
            return std::nullopt;
        }
        if (!m_settings->archivesonly() && is_matching_file(filename, filetype, &fpstat)) {
            return std::optional<FileResult*>{fileresult};
        }
        return std::nullopt;
    }

    std::vector<FileResult*> Finder::get_file_results(const std::string& filepath) {
        boost::filesystem::path p(filepath);
        std::vector<std::string> finddirs{};
        std::vector<FileResult*> fileresults{};

        std::vector<boost::filesystem::directory_entry> dir_entries;
        copy(boost::filesystem::directory_iterator(p), boost::filesystem::directory_iterator(),
             back_inserter(dir_entries));

        for (const auto& de : dir_entries) {
            const boost::filesystem::path& subpath = de.path();
            if (boost::filesystem::is_directory(subpath) && m_settings->recursive() && is_matching_dir(subpath.string())) {
                finddirs.push_back(subpath.string());
            } else if (boost::filesystem::is_regular_file(subpath)) {
                std::optional<FileResult*> optFileResult = filter_to_file_result(subpath.string());
                if (optFileResult.has_value()) {
                    fileresults.push_back(optFileResult.value());
                }
            }
        }

        for (const auto& finddir : finddirs) {
            std::vector<FileResult*> subfileresults = get_file_results(finddir);
            fileresults.insert(fileresults.end(), subfileresults.begin(), subfileresults.end());
        }

        return fileresults;
    }

    // std::vector<FindResult*> Finder::find_path(const std::string& filepath) {
    //     std::vector<FindResult*> results = {};
    //     std::vector<FileResult*> findfiles = get_file_results(filepath);

    //     // sort using a lambda expression
    //     std::sort(findfiles.begin(), findfiles.end(), [](FileResult* sf1, FileResult* sf2) {
    //         if (sf1->path() == sf2->path()) {
    //             return sf1->filename() < sf2->filename();
    //         }
    //         return sf1->path() < sf2->path();
    //     });

    //     if (m_settings->verbose()) {
    //         std::set<std::string> finddir_set = {};
    //         for (const auto& findfile : findfiles) {
    //             finddir_set.insert(findfile->path());
    //         }
    //         std::vector<std::string> finddirs(finddir_set.begin(), finddir_set.end());

    //         std::string msg = "\nDirectories to be found (";
    //         msg.append(std::to_string(finddirs.size())).append("):");
    //         log(msg);
    //         for (const auto& finddir : finddirs) {
    //             log(finddir);
    //         }

    //         msg = "\nFiles to be found (";
    //         msg.append(std::to_string(findfiles.size())).append("):");
    //         log(msg);
    //         for (const auto& findfile : findfiles) {
    //             log(findfile->string());
    //         }
    //     }

    //     for (const auto& findfile : findfiles) {
    //         std::vector<FindResult*> fileresults = find_file(findfile);
    //         if (!fileresults.empty()) {
    //             results.insert(results.end(), fileresults.begin(), fileresults.end());
    //         }
    //     }
    //     return results;
    // }
}
