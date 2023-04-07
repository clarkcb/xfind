#include <boost/algorithm/string/case_conv.hpp>
#include "common.h"
#include "FileUtil.h"
#include "StringUtil.h"
#include "FindSettings.h"

namespace cppfind {
    FindSettings::FindSettings() {
        m_in_archiveextensions = {};
        m_in_archivefilepatterns = {};
        m_in_dirpatterns = {};
        m_in_extensions = {};
        m_in_filepatterns = {};
        m_in_filetypes = {};
        m_out_archiveextensions = {};
        m_out_archivefilepatterns = {};
        m_out_dirpatterns = {};
        m_out_extensions = {};
        m_out_filepatterns = {};
        m_out_filetypes = {};
        m_paths = {};
        m_sortby = SortBy::FILEPATH;
    }

    void FindSettings::add_pattern(const std::string& p, std::vector<FindPattern*>* ps) {
        ps->push_back(new FindPattern(p));
    }

    void FindSettings::add_extensions(const std::string& exts, std::vector<std::string>* extensions) {
        std::vector<std::string> xs = StringUtil::split_string(exts, ",");
        for (const auto& x : xs) {
            if (!x.empty()) {
                extensions->push_back(x);
            }
        }
    }

    void FindSettings::add_in_archiveextension(const std::string& ext) {
        add_extensions(ext, &m_in_archiveextensions);
    }

    void FindSettings::add_in_archivefilepattern(const std::string& p) {
        add_pattern(p, &m_in_archivefilepatterns);
    }

    void FindSettings::add_in_dirpattern(const std::string& p) {
        add_pattern(p, &m_in_dirpatterns);
    }

    void FindSettings::add_in_extension(const std::string& ext) {
        add_extensions(ext, &m_in_extensions);
    }

    void FindSettings::add_in_filepattern(const std::string& p) {
        add_pattern(p, &m_in_filepatterns);
    }

    void FindSettings::add_in_filetype(const FileType filetype) {
        m_in_filetypes.push_back(filetype);
    }

    void FindSettings::add_out_archiveextension(const std::string& ext) {
        add_extensions(ext, &m_out_archiveextensions);
    }

    void FindSettings::add_out_archivefilepattern(const std::string& p) {
        add_pattern(p, &m_out_archivefilepatterns);
    }

    void FindSettings::add_out_dirpattern(const std::string& p) {
        add_pattern(p, &m_out_dirpatterns);
    }

    void FindSettings::add_out_extension(const std::string& ext) {
        add_extensions(ext, &m_out_extensions);
    }

    void FindSettings::add_out_filepattern(const std::string& p) {
        add_pattern(p, &m_out_filepatterns);
    }

    void FindSettings::add_out_filetype(const FileType filetype) {
        m_out_filetypes.push_back(filetype);
    }

    void FindSettings::add_path(const std::string& p) {
        m_paths.push_back(p);
    }

    bool FindSettings::archivesonly() const {
        return m_archivesonly;
    }

    bool FindSettings::debug() const {
        return m_debug;
    }

    bool FindSettings::excludehidden() const {
        return m_excludehidden;
    }

    bool FindSettings::includearchives() const {
        return m_includearchives;
    }

    bool FindSettings::listdirs() const {
        return m_listdirs;
    }

    bool FindSettings::listfiles() const {
        return m_listfiles;
    }

    long FindSettings::maxlastmod() const {
        return m_maxlastmod;
    }

    long FindSettings::maxsize() const {
        return m_maxsize;
    }

    long FindSettings::minlastmod() const {
        return m_minlastmod;
    }

    long FindSettings::minsize() const {
        return m_minsize;
    }

    bool FindSettings::printusage() const {
        return m_printusage;
    }

    bool FindSettings::printversion() const {
        return m_printversion;
    }

    bool FindSettings::recursive() const {
        return m_recursive;
    }

    bool FindSettings::sort_caseinsensitive() const {
        return m_sort_caseinsensitive;
    }

    bool FindSettings::sort_descending() const {
        return m_sort_descending;
    }

    bool FindSettings::verbose() const {
        return m_verbose;
    }

    std::vector<std::string>* FindSettings::in_archiveextensions() {
        return &m_in_archiveextensions;
    }

    std::vector<FindPattern*>* FindSettings::in_archivefilepatterns() {
        return &m_in_archivefilepatterns;
    }

    std::vector<FindPattern*>* FindSettings::in_dirpatterns() {
        return &m_in_dirpatterns;
    }

    std::vector<std::string>* FindSettings::in_extensions() {
        return &m_in_extensions;
    }

    std::vector<FindPattern*>* FindSettings::in_filepatterns() {
        return &m_in_filepatterns;
    }

    std::vector<FileType>* FindSettings::in_filetypes() {
        return &m_in_filetypes;
    }

    std::vector<std::string>* FindSettings::out_archiveextensions() {
        return &m_out_archiveextensions;
    }

    std::vector<FindPattern*>* FindSettings::out_archivefilepatterns() {
        return &m_out_archivefilepatterns;
    }

    std::vector<FindPattern*>* FindSettings::out_dirpatterns() {
        return &m_out_dirpatterns;
    }

    std::vector<std::string>* FindSettings::out_extensions() {
        return &m_out_extensions;
    }

    std::vector<FindPattern*>* FindSettings::out_filepatterns() {
        return &m_out_filepatterns;
    }

    std::vector<FileType>* FindSettings::out_filetypes() {
        return &m_out_filetypes;
    }

    std::vector<std::string>* FindSettings::paths() {
        return &m_paths;
    }

    SortBy FindSettings::sortby() {
        return m_sortby;
    }

    void FindSettings::archivesonly(const bool b) {
        m_archivesonly = b;
        if (b) m_includearchives = b;
    }

    void FindSettings::debug(const bool b) {
        m_debug = b;
        if (b) m_verbose = b;
    }

    void FindSettings::excludehidden(const bool b) {
        m_excludehidden = b;
    }

    void FindSettings::includearchives(const bool b) {
        m_includearchives = b;
    }

    void FindSettings::listdirs(const bool b) {
        m_listdirs = b;
    }

    void FindSettings::listfiles(const bool b) {
        m_listfiles = b;
    }

    void FindSettings::maxlastmod(const long maxlastmod) {
        m_maxlastmod = maxlastmod;
    }

    void FindSettings::maxsize(const long maxsize) {
        m_maxsize = maxsize;
    }

    void FindSettings::minlastmod(const long minlastmod) {
        m_minlastmod = minlastmod;
    }

    void FindSettings::minsize(const long minsize) {
        m_minsize = minsize;
    }

    void FindSettings::printusage(const bool b) {
        m_printusage = b;
    }

    void FindSettings::printversion(const bool b) {
        m_printversion = b;
    }

    void FindSettings::recursive(const bool b) {
        m_recursive = b;
    }

    void FindSettings::sortby(const SortBy sortby) {
        m_sortby = sortby;
    }

    void FindSettings::set_sortby(const std::string& name) {
        m_sortby = FindSettings::sortby_from_name(name);
    }

    void FindSettings::sort_caseinsensitive(const bool b) {
        m_sort_caseinsensitive = b;
    }

    void FindSettings::sort_descending(const bool b) {
        m_sort_descending = b;
    }

    void FindSettings::verbose(const bool b) {
        m_verbose = b;
    }

    std::string FindSettings::bool_to_string(bool b) {
        return b ? "true" : "false";
    }

    std::string FindSettings::string_vector_to_string(std::vector<std::string>* ss) {
        std::string ss_string = "[";
        int count = 0;
        for (auto const& s : *ss) {
            if (count > 0) {
                ss_string.append(", ");
            }
            ss_string.append("\"").append(s).append("\"");
            count++;
        }
        ss_string.append("]");
        return ss_string;
    }

    std::string FindSettings::findpatterns_to_string(std::vector<FindPattern*>* ps) {
        std::string ps_string = "[";
        int count = 0;
        for (auto const& p : *ps) {
            if (count > 0) {
                ps_string.append(", ");
            }
            ps_string.append("\"").append(p->pattern()).append("\"");
            count++;
        }
        ps_string.append("]");
        return ps_string;
    }

    std::string FindSettings::filetypes_to_string(std::vector<FileType>* ts) {
        std::string ts_string = "[";
        int count = 0;
        for (auto const& t : *ts) {
            if (count > 0) {
                ts_string.append(", ");
            }
            ts_string.append("\"").append(FileTypes::to_name(t)).append("\"");
            count++;
        }
        ts_string.append("]");
        return ts_string;
    }

    SortBy FindSettings::sortby_from_name(const std::string& name) {
        std::string uname = boost::to_upper_copy(name);
        if (uname == "PATH") {
            return SortBy::FILEPATH;
        }
        if (uname == "NAME") {
            return SortBy::FILENAME;
        }
        if (uname == "SIZE") {
            return SortBy::FILESIZE;
        }
        if (uname == "TYPE") {
            return SortBy::FILETYPE;
        }
        if (uname == "LASTMOD") {
            return SortBy::LASTMOD;
        }
        return SortBy::FILEPATH;
    }

    std::string FindSettings::sortby_to_name(const SortBy sortby) {
        switch (sortby)
        {
            case SortBy::FILEPATH:
                return "FILEPATH";
            case SortBy::FILENAME:
                return "FILENAME";
            case SortBy::FILESIZE:
                return "FILESIZE";
            case SortBy::FILETYPE:
                return "FILETYPE";
            case SortBy::LASTMOD:
                return "LASTMOD";
            default:
                return "UNKNOWN";
        }
    }

    std::string FindSettings::string() {
        auto settings_str =
                std::string("FindSettings(")
                + "archivesonly: " + bool_to_string(m_archivesonly)
                + ", debug: " + bool_to_string(m_debug)
                + ", excludehidden: " + bool_to_string(m_excludehidden)
                + ", in_archiveextensions: " + string_vector_to_string(&m_in_archiveextensions)
                + ", in_archivefilepatterns: " + findpatterns_to_string(&m_in_archivefilepatterns)
                + ", in_dirpatterns: " + findpatterns_to_string(&m_in_dirpatterns)
                + ", in_extensions: " + string_vector_to_string(&m_in_extensions)
                + ", in_filepatterns: " + findpatterns_to_string(&m_in_filepatterns)
                + ", in_filetypes: " + filetypes_to_string(&m_in_filetypes)
                + ", includearchives: " + bool_to_string(m_includearchives)
                + ", listdirs: " + bool_to_string(m_listdirs)
                + ", listfiles: " + bool_to_string(m_listfiles)
                + ", maxlastmod: \"" + long_to_datestr(m_maxlastmod) + "\""
                + ", maxsize: " + std::to_string(m_maxsize)
                + ", minlastmod: \"" + long_to_datestr(m_minlastmod) + "\""
                + ", minsize: " + std::to_string(m_minsize)
                + ", out_archiveextensions: " + string_vector_to_string(&m_out_archiveextensions)
                + ", out_archivefilepatterns: " + findpatterns_to_string(&m_out_archivefilepatterns)
                + ", out_dirpatterns: " + findpatterns_to_string(&m_out_dirpatterns)
                + ", out_extensions: " + string_vector_to_string(&m_out_extensions)
                + ", out_filepatterns: " + findpatterns_to_string(&m_out_filepatterns)
                + ", out_filetypes: " + filetypes_to_string(&m_out_filetypes)
                + ", paths: " + string_vector_to_string(&m_paths)
                + ", printusage: " + bool_to_string(m_printusage)
                + ", printversion: " + bool_to_string(m_printversion)
                + ", recursive: " + bool_to_string(m_recursive)
                + ", sortby: " + sortby_to_name(m_sortby)
                + ", sort_caseinsensitive: " + bool_to_string(m_sort_caseinsensitive)
                + ", sort_descending: " + bool_to_string(m_sort_descending)
                + ", verbose: " + bool_to_string(m_verbose)
                + ")";
        return settings_str;
    }
}
