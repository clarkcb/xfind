#include <algorithm>
#include <boost/filesystem.hpp>
#include <iostream>
#include <optional>
#include "common.h"
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
            std::string expanded = FileUtil::expand_path(p);
            if (!FileUtil::file_exists(p) && !FileUtil::file_exists(expanded)) {
                throw FindException("Startpath not found");
            }
        }
    }

    FindFile* Finder::get_findfile(std::string& filepath) {
        FileType filetype = m_filetypes->get_filetype(filepath);
        boost::filesystem::path path(filepath);
        std::string parent_path = path.parent_path().string();
        std::string filename = path.filename().string();
        return new FindFile(parent_path, filename, filetype);
    }

    std::vector<FindFile*> Finder::find() {
        std::vector<FindFile*> findfiles{};

        for (auto& p : *m_settings->paths()) {
            std::string expanded = FileUtil::expand_path(p);

            if (FileUtil::is_directory(p) || FileUtil::is_directory(expanded)) {
                std::vector<FindFile*> p_files{};
                if (FileUtil::is_directory(p)) {
                    p_files = get_find_files(p);
                } else {
                    p_files = get_find_files(expanded);
                }
                findfiles.insert(findfiles.end(), p_files.begin(), p_files.end());

            } else if (FileUtil::is_regular_file(p)) {
                auto* sf = get_findfile(p);
                findfiles.push_back(sf);

            } else if (FileUtil::is_regular_file(expanded)) {
                auto* sf = get_findfile(expanded);
                findfiles.push_back(sf);

            } else {
                throw FindException("path is an unsupported file type");
            }
        }

        return findfiles;
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

    bool Finder::is_find_dir(const std::string& filepath) {
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

    bool Finder::is_archive_find_file(const std::string& filename) {
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

    bool Finder::is_find_file(const std::string& filename, const FileType filetype) {
        std::string ext = FileUtil::get_extension(filename);
        std::vector <std::string>* in_exts = m_settings->in_extensions();
        std::vector <std::string>* out_exts = m_settings->out_extensions();
        if ((!in_exts->empty() && std::find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
            || (!out_exts->empty() && std::find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
            return false;
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
        return true;
    }

    bool Finder::filter_file(const std::string& filepath) {
        boost::filesystem::path p(filepath);
        std::string filename = p.filename().string();
        if (m_settings->excludehidden() && FileUtil::is_hidden(filename)) {
            return false;
        }
        auto filetype = m_filetypes->get_filetype(filename);
        if (filetype == FileType::ARCHIVE) {
            return m_settings->includearchives() && is_archive_find_file(filename);
        }
        return !m_settings->archivesonly() && is_find_file(filename, filetype);
    }

    // std::optional<FindFile*> Finder::filter_to_find_file(const std::string& filepath) {
    //     boost::filesystem::path p(filepath);
    //     std::string filename = p.filename().string();
    //     if (m_settings->excludehidden() && FileUtil::is_hidden(filename)) {
    //         return false;
    //     }
    //     std::string parent_path = subpath.parent_path().string();
    //     auto filetype = m_filetypes->get_filetype(filename);
    //     auto findfile = new FindFile(parent_path, filename, filetype);
    //     if (filetype == FileType::ARCHIVE) {
    //         if (m_settings->includearchives() && is_archive_find_file(filename)) {
    //             return std::optional<FindFile*>{findfile};
    //         }
    //         return std::nullopt;
    //     }
    //     if (!m_settings->archivesonly() && is_find_file(filename)) {
    //         return std::optional<FindFile*>{findfile};
    //     }
    //     return std::nullopt;
    // }

    std::vector<FindFile*> Finder::get_find_files(const std::string& filepath) {
        boost::filesystem::path p(filepath);
        std::vector<std::string> finddirs = {};
        std::vector<FindFile*> findfiles = {};

        std::vector<boost::filesystem::directory_entry> v;
        copy(boost::filesystem::directory_iterator(p), boost::filesystem::directory_iterator(), back_inserter(v));

        for (std::vector<boost::filesystem::directory_entry>::const_iterator it = v.begin(); it != v.end(); ++it) {
            boost::filesystem::path subpath = (*it).path();
            if (boost::filesystem::is_directory(subpath) && m_settings->recursive() && is_find_dir(subpath.string())) {
                finddirs.push_back(subpath.string());
            } else if (boost::filesystem::is_regular_file(subpath)) {
                std::string parent_path = subpath.parent_path().string();
                std::string filename = subpath.filename().string();
                FileType filetype = m_filetypes->get_filetype(filename);
                if (filter_file(filename)) {
                    findfiles.push_back(new FindFile(parent_path, filename, filetype));
                }
            }
        }

        for (const auto& finddir : finddirs) {
            std::vector<FindFile*> subfindfiles = get_find_files(finddir);
            findfiles.insert(findfiles.end(), subfindfiles.begin(), subfindfiles.end());
        }

        return findfiles;
    }

    // std::vector<FindResult*> Finder::find_path(const std::string& filepath) {
    //     std::vector<FindResult*> results = {};
    //     std::vector<FindFile*> findfiles = get_find_files(filepath);

    //     // sort using a lambda expression
    //     std::sort(findfiles.begin(), findfiles.end(), [](FindFile* sf1, FindFile* sf2) {
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
