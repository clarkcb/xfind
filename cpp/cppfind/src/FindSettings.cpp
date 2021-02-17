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
        m_findpatterns = {};
        m_startpath = nullptr;
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

    void FindSettings::add_in_linesafterpattern(const std::string& p) {
        add_pattern(p, &m_in_linesafterpatterns);
    }

    void FindSettings::add_in_linesbeforepattern(const std::string& p) {
        add_pattern(p, &m_in_linesbeforepatterns);
    }

    void FindSettings::add_linesaftertopattern(const std::string& p) {
        add_pattern(p, &m_linesaftertopatterns);
    }

    void FindSettings::add_linesafteruntilpattern(const std::string& p) {
        add_pattern(p, &m_linesafteruntilpatterns);
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

    void FindSettings::add_out_linesafterpattern(const std::string& p) {
        add_pattern(p, &m_out_linesafterpatterns);
    }

    void FindSettings::add_out_linesbeforepattern(const std::string& p) {
        add_pattern(p, &m_out_linesbeforepatterns);
    }

    void FindSettings::add_findpattern(const std::string& p) {
        add_pattern(p, &m_findpatterns);
    }

    bool FindSettings::archivesonly() const {
        return m_archivesonly;
    }

    bool FindSettings::colorize() const {
        return m_colorize;
    }

    bool FindSettings::debug() const {
        return m_debug;
    }

    bool FindSettings::excludehidden() const {
        return m_excludehidden;
    }

    bool FindSettings::firstmatch() const {
        return m_firstmatch;
    }

    bool FindSettings::multilineoption-REMOVE() const {
        return m_multilineoption-REMOVE;
    }

    unsigned int FindSettings::linesafter() const {
        return m_linesafter;
    }

    unsigned int FindSettings::linesbefore() const {
        return m_linesbefore;
    }

    bool FindSettings::listdirs() const {
        return m_listdirs;
    }

    bool FindSettings::listfiles() const {
        return m_listfiles;
    }

    bool FindSettings::listlines() const {
        return m_listlines;
    }

    size_t FindSettings::maxlinelength() const {
        return m_maxlinelength;
    }

    bool FindSettings::printresults() const {
        return m_printresults;
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

    bool FindSettings::findarchives() const {
        return m_findarchives;
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

    std::vector<FindPattern*>* FindSettings::in_linesafterpatterns() {
        return &m_in_linesafterpatterns;
    }

    std::vector<FindPattern*>* FindSettings::in_linesbeforepatterns() {
        return &m_in_linesbeforepatterns;
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

    std::vector<FindPattern*>* FindSettings::out_linesafterpatterns() {
        return &m_out_linesafterpatterns;
    }

    std::vector<FindPattern*>* FindSettings::out_linesbeforepatterns() {
        return &m_out_linesbeforepatterns;
    }

    std::vector<FindPattern*>* FindSettings::findpatterns() {
        return &m_findpatterns;
    }


    std::string* FindSettings::startpath() {
        return m_startpath;
    }

    bool FindSettings::uniquelines() const {
        return m_uniquelines;
    }

    bool FindSettings::verbose() const {
        return m_verbose;
    }

    void FindSettings::archivesonly(const bool b) {
        m_archivesonly = b;
        if (b) m_findarchives = b;
    }

    void FindSettings::colorize(const bool b) {
        m_colorize = b;
    }

    void FindSettings::debug(const bool b) {
        m_debug = b;
        if (b) m_verbose = b;
    }

    void FindSettings::excludehidden(const bool b) {
        m_excludehidden = b;
    }

    void FindSettings::firstmatch(const bool b) {
        m_firstmatch = b;
    }

    void FindSettings::linesafter(const unsigned int linecount) {
        m_linesafter = linecount;
    }

    void FindSettings::linesbefore(const unsigned int linecount) {
        m_linesbefore = linecount;
    }

    void FindSettings::listdirs(const bool b) {
        m_listdirs = b;
    }

    void FindSettings::listfiles(const bool b) {
        m_listfiles = b;
    }

    void FindSettings::listlines(const bool b) {
        m_listlines = b;
    }

    void FindSettings::maxlinelength(const size_t max) {
        m_maxlinelength = max;
    }

    void FindSettings::multilineoption-REMOVE(const bool b) {
        m_multilineoption-REMOVE = b;
    }

    void FindSettings::printresults(const bool b) {
        m_printresults = b;
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

    void FindSettings::findarchives(const bool b) {
        m_findarchives = b;
    }

    void FindSettings::startpath(std::string& s) {
        m_startpath = &s;
    }

    void FindSettings::uniquelines(const bool b) {
        m_uniquelines = b;
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

    std::string FindSettings::string() {
        std::string path = "\"\"";
        if (m_startpath != nullptr) {
            path = std::string("\"") + *m_startpath + "\"";
        }
        auto settings_str =
                std::string("FindSettings(")
                + "archivesonly: " + bool_to_string(m_archivesonly)
                + ", colorize: " + bool_to_string(m_colorize)
                + ", debug: " + bool_to_string(m_debug)
                + ", excludehidden: " + bool_to_string(m_excludehidden)
                + ", firstmatch: " + bool_to_string(m_firstmatch)
                + ", in_archiveextensions: " + string_vector_to_string(&m_in_archiveextensions)
                + ", in_archivefilepatterns: " + findpatterns_to_string(&m_in_archivefilepatterns)
                + ", in_dirpatterns: " + findpatterns_to_string(&m_in_dirpatterns)
                + ", in_extensions: " + string_vector_to_string(&m_in_extensions)
                + ", in_filepatterns: " + findpatterns_to_string(&m_in_filepatterns)
                + ", in_linesafterpatterns: " + findpatterns_to_string(&m_in_linesafterpatterns)
                + ", in_linesbeforepatterns: " + findpatterns_to_string(&m_in_linesbeforepatterns)
                + ", linesafter: " + std::to_string(m_linesafter)
                + ", linesaftertopatterns: " + findpatterns_to_string(&m_linesaftertopatterns)
                + ", linesafteruntilpatterns: " + findpatterns_to_string(&m_linesafteruntilpatterns)
                + ", linesbefore: " + std::to_string(m_linesbefore)
                + ", listdirs: " + bool_to_string(m_listdirs)
                + ", listfiles: " + bool_to_string(m_listfiles)
                + ", listlines: " + bool_to_string(m_listlines)
                + ", maxlinelength: " + std::to_string(m_maxlinelength)
                + ", multilineoption-REMOVE: " + bool_to_string(m_multilineoption-REMOVE)
                + ", out_archiveextensions: " + string_vector_to_string(&m_out_archiveextensions)
                + ", out_archivefilepatterns: " + findpatterns_to_string(&m_out_archivefilepatterns)
                + ", out_dirpatterns: " + findpatterns_to_string(&m_out_dirpatterns)
                + ", out_extensions: " + string_vector_to_string(&m_out_extensions)
                + ", out_filepatterns: " + findpatterns_to_string(&m_out_filepatterns)
                + ", out_linesafterpatterns: " + findpatterns_to_string(&m_out_linesafterpatterns)
                + ", out_linesbeforepatterns: " + findpatterns_to_string(&m_out_linesbeforepatterns)
                + ", printresults: " + bool_to_string(m_printresults)
                + ", printusage: " + bool_to_string(m_printusage)
                + ", printversion: " + bool_to_string(m_printversion)
                + ", recursive: " + bool_to_string(m_recursive)
                + ", findarchives: " + bool_to_string(m_findarchives)
                + ", findpatterns: " + findpatterns_to_string(&m_findpatterns)
                + ", startpath: " + path
                + ", uniquelines: " + bool_to_string(m_uniquelines)
                + ", verbose: " + bool_to_string(m_verbose)
                + ")";
        return settings_str;
    }

    std::ostream& operator<<(std::ostream& strm, FindSettings& settings) {
        std::string settings_string = settings.string();
        return strm << settings_string;
    }
}
