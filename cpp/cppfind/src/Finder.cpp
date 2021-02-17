#include <algorithm>
#include <boost/filesystem.hpp>
#include <iostream>
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
        std::string* startpath = ss->startpath();
        if (startpath == nullptr || startpath->empty()) {
            throw FindException("Startpath not defined");
        }
        std::string expanded = FileUtil::expand_path(*startpath);
        if (!FileUtil::file_exists(*startpath) && !FileUtil::file_exists(expanded)) {
            throw FindException("Startpath not found");
        }
        if (ss->findpatterns()->empty()) {
            throw FindException("No find patterns defined");
        }
    }

    FindFile* Finder::get_findfile(std::string& filepath) {
        FileType filetype = m_filetypes->get_filetype(filepath);
        boost::filesystem::path path(filepath);
        std::string parent_path = path.parent_path().string();
        std::string filename = path.filename().string();
        return new FindFile(parent_path, filename, filetype);
    }

    std::vector<FindResult*> Finder::find() {
        std::string* startpath = m_settings->startpath();
        std::string expanded = FileUtil::expand_path(*startpath);
        if (FileUtil::is_directory(*startpath)) {
            return find_path(*startpath);

        } else if (FileUtil::is_directory(expanded)) {
            return find_path(expanded);

        } else if (FileUtil::is_regular_file(*startpath)) {
            auto* sf = get_findfile(*startpath);
            return find_file(sf);

        } else if (FileUtil::is_regular_file(expanded)) {
            auto* sf = get_findfile(expanded);
            return find_file(sf);

        } else {
            throw FindException("m_startpath is an unsupported file type");
        }
    }

    bool matches_any_pattern(const std::string& s, const std::vector<FindPattern*>& patterns) {
        std::smatch pmatch;
        for (auto& p : patterns) {
            if (regex_find(s, pmatch, p->r())) {
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
            } else if (boost::filesystem::is_regular_file(subpath) && filter_file(subpath.string())) {
                std::string parent_path = subpath.parent_path().string();
                std::string filename = subpath.filename().string();
                FileType filetype = m_filetypes->get_filetype(filename);
                findfiles.push_back(new FindFile(parent_path, filename, filetype));
            }
        }

        for (const auto& finddir : finddirs) {
            std::vector<FindFile*> subfindfiles = get_find_files(finddir);
            findfiles.insert(findfiles.end(), subfindfiles.begin(), subfindfiles.end());
        }

        return findfiles;
    }

    bool Finder::is_find_file(const std::string& filename) {
        std::string ext = FileUtil::get_extension(filename);
        std::vector <std::string>* in_exts = m_settings->in_extensions();
        std::vector <std::string>* out_exts = m_settings->out_extensions();
        if ((!in_exts->empty() && find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
            || (!out_exts->empty() && find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
            return false;
        }
        std::vector<FindPattern*>* in_filepatterns = m_settings->in_filepatterns();
        std::vector<FindPattern*>* out_filepatterns = m_settings->out_filepatterns();
        return ((in_filepatterns->empty() || matches_any_pattern(filename, *in_filepatterns))
                && (out_filepatterns->empty() || !matches_any_pattern(filename, *out_filepatterns)));
    }

    bool Finder::is_archive_find_file(const std::string& filename) {
        std::string ext = FileUtil::get_extension(filename);
        std::vector <std::string>* in_exts = m_settings->in_archiveextensions();
        std::vector <std::string>* out_exts = m_settings->out_archiveextensions();
        if ((!in_exts->empty() && find(in_exts->begin(), in_exts->end(), ext) == in_exts->end())
            || (!out_exts->empty() && find(out_exts->begin(), out_exts->end(), ext) != out_exts->end())) {
            return false;
        }
        std::vector<FindPattern*>* in_filepatterns = m_settings->in_archivefilepatterns();
        std::vector<FindPattern*>* out_filepatterns = m_settings->out_archivefilepatterns();
        return ((in_filepatterns->empty() || matches_any_pattern(filename, *in_filepatterns))
                && (out_filepatterns->empty() || !matches_any_pattern(filename, *out_filepatterns)));
    }

    bool Finder::filter_file(std::string filepath) {
        boost::filesystem::path p(filepath);
        std::string filename = p.filename().string();
        if (FileUtil::is_hidden(filename) && m_settings->excludehidden()) {
            return false;
        }
        if (m_filetypes->get_filetype(filename) == FileType::ARCHIVE) {
            return m_settings->findarchives() && is_archive_find_file(filename);
        }
        return !m_settings->archivesonly() && is_find_file(filename);
    }

    std::vector<FindResult*> Finder::find_path(const std::string& filepath) {
        std::vector<FindResult*> results = {};
        std::vector<FindFile*> findfiles = get_find_files(filepath);

        // sort using a lambda expression
        std::sort(findfiles.begin(), findfiles.end(), [](FindFile* sf1, FindFile* sf2) {
            if (sf1->path() == sf2->path()) {
                return sf1->filename() < sf2->filename();
            }
            return sf1->path() < sf2->path();
        });

        if (m_settings->verbose()) {
            std::set<std::string> finddir_set = {};
            for (const auto& findfile : findfiles) {
                finddir_set.insert(findfile->path());
            }
            std::vector<std::string> finddirs(finddir_set.begin(), finddir_set.end());

            std::string msg = "\nDirectories to be found (";
            msg.append(std::to_string(finddirs.size())).append("):");
            log(msg);
            for (const auto& finddir : finddirs) {
                log(finddir);
            }

            msg = "\nFiles to be found (";
            msg.append(std::to_string(findfiles.size())).append("):");
            log(msg);
            for (const auto& findfile : findfiles) {
                log(findfile->string());
            }
        }

        for (const auto& findfile : findfiles) {
            std::vector<FindResult*> fileresults = find_file(findfile);
            if (!fileresults.empty()) {
                results.insert(results.end(), fileresults.begin(), fileresults.end());
            }
        }
        return results;
    }

    std::vector<FindResult*> Finder::find_file(FindFile* sf) {
        std::vector<FindResult*> results = {};
        if (sf->filetype() == FileType::CODE || sf->filetype() == FileType::XML || sf->filetype() == FileType::TEXT) {
            results = find_text_file(sf);
        } else if (sf->filetype() == FileType::BINARY) {
            results = find_binary_file(sf);
        } else if (sf->filetype() == FileType::ARCHIVE) {
            //cout << "m_findfile is an ARCHIVE file: " << sf->filename() << endl;
        } else {
            std::cout << "m_findfile is an UNKNOWN file: " << sf->filename() << std::endl;
        }
        return results;
    }

    std::vector<FindResult*> Finder::find_text_file(FindFile* sf) {
        if (m_settings->debug()) {
            std::cout << "Finding text file " << sf->string() << std::endl;
        }
        std::ifstream fin(sf->string());
        std::vector<FindResult*> results = find_ifstream(fin);
        fin.close();

        for (const auto& r : results) {
            r->set_findfile(sf);
        }
        return results;
    }

    std::vector<FindResult*> Finder::find_ifstream(std::ifstream& fin) {
        if (m_settings->multilineoption-REMOVE()) {
            return find_ifstream_contents(fin);
        } else {
            return find_ifstream_lines(fin);
        }
    }

    bool lines_match(std::vector<std::string>& lines, std::vector<FindPattern*>* in_patterns,
                     std::vector<FindPattern*>* out_patterns) {
        return lines.empty() ||
               ((in_patterns->empty() || any_matches_any_pattern(lines, *in_patterns)) &&
                (out_patterns->empty() || !any_matches_any_pattern(lines, *out_patterns)));
    }

    std::vector<FindResult*> Finder::find_ifstream_lines(std::ifstream& fin) {
        std::vector<FindResult*> results = {};

        for (const auto& p : *(m_settings->findpatterns())) {

            fin.seekg(0);

            bool found_pattern = false;

            int linenum = 0;
            std::string line;

            std::deque<std::string> lines_before;
            std::deque<std::string> lines_after;
            unsigned int lines_before_count = m_settings->linesbefore();
            unsigned int lines_after_count = m_settings->linesafter();

            while (true) {
                if (m_settings->firstmatch() && found_pattern) {
                    break;
                }

                ++linenum;
                if (!lines_after.empty()) {
                    line = lines_after.front();
                    lines_after.pop_front();
                } else if (getline(fin, line)) {
                    // nothing to do, action in if clause
                } else {
                    break;
                }

                if (lines_after_count > 0) {
                    std::string next_line;
                    while (lines_after.size() < lines_after_count && getline(fin, next_line)) {
                        lines_after.push_back(next_line);
                    }
                }

                auto matches_begin = std::sregex_iterator(line.begin(), line.end(), p->r());
                auto matches_end = std::sregex_iterator();

                for (std::sregex_iterator it = matches_begin; it != matches_end; ++it) {
                    if (m_settings->firstmatch() && found_pattern) {
                        break;
                    }

                    std::smatch match = *it;

                    unsigned long match_start_idx = match.position(0);
                    unsigned long match_end_idx = match_start_idx + match.length(0);
                    auto* v_lines_before = new std::vector<std::string>(lines_before.begin(), lines_before.end());
                    if (!lines_match(*v_lines_before, m_settings->in_linesbeforepatterns(),
                                     m_settings->out_linesbeforepatterns())) {
                        continue;
                    }
                    auto* v_lines_after = new std::vector<std::string>(lines_after.begin(), lines_after.end());
                    if (!lines_match(*v_lines_after, m_settings->in_linesafterpatterns(),
                                     m_settings->out_linesafterpatterns())) {
                        continue;
                    }

                    results.push_back(new FindResult(p, nullptr, linenum,
                                                       match_start_idx + 1,
                                                       match_end_idx + 1,
                                                       line, v_lines_before, v_lines_after));

                    if (m_settings->firstmatch()) {
                        found_pattern = true;
                        break;
                    }
                }

                if (lines_before_count > 0) {
                    if (lines_before.size() == lines_before_count) {
                        lines_before.pop_front();
                    }
                    if (lines_before.size() < lines_before_count) {
                        lines_before.push_back(line);
                    }
                }
            }
        }
        return results;
    }

    std::vector<FindResult*> Finder::find_ifstream_contents(std::ifstream& fin) {
        std::string contents = FileUtil::get_contents(fin);
        return find_multiline_string(contents);
    }

    std::vector<unsigned long> get_newline_indices(std::string& s) {
        std::vector<unsigned long> newline_indices = {};
        for (unsigned long i=0; i < s.length(); i++) {
            if (s.at(i) == '\n') {
                newline_indices.push_back(i);
            }
        }
        return newline_indices;
    }

    unsigned long get_linenum_for_pos(std::vector<unsigned long>& newline_indices, unsigned long pos) {
        long i = 0;
        while (newline_indices[i] < pos) i++;
        return ++i;
    }

    std::pair<unsigned long, unsigned long> get_linestartend_for_pos(std::string& s,
                                                                     std::vector<unsigned long>& newline_indices,
                                                                     unsigned long pos) {
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        unsigned long line_start_idx = newline_indices[i-1] + 1;
        unsigned long line_end_idx = newline_indices[i];
        return std::make_pair(line_start_idx, line_end_idx);
    }

    std::string get_line_for_pos(std::string& s, std::vector<unsigned long> newline_indices, unsigned long pos) {
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        unsigned long line_start_idx = newline_indices[i-1] + 1;
        unsigned long line_len = newline_indices[i] - newline_indices[i-1] - 1;
        return s.substr(line_start_idx, line_len);
    }

    std::vector<std::string> get_lines_before_pos(std::string& s, std::vector<unsigned long> newline_indices,
                                                  unsigned long line_count, unsigned long pos) {
        std::vector<std::string> lines;
        lines.reserve(line_count);
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        --i;
        while (i > 0 && line_count > 0) {
            std::pair<unsigned long, unsigned long> startend =
                    get_linestartend_for_pos(s, newline_indices, newline_indices[i-1]);
            unsigned long line_start_idx = startend.first;
            unsigned long line_end_idx = startend.second;
            lines.insert(lines.begin(), s.substr(line_start_idx, line_end_idx - line_start_idx));
            --i;
            --line_count;
        }
        return lines;
    }

    std::vector<std::string> get_lines_after_pos(std::string& s, std::vector<unsigned long> newline_indices,
                                                 unsigned long line_count, unsigned long pos) {
        std::vector<std::string> lines;
        lines.reserve(line_count);
        long i = 0;
        while (newline_indices[i] <= pos) i++;
        while (i < newline_indices.size() && line_count > 0) {
            std::pair<unsigned long, unsigned long> startend =
                    get_linestartend_for_pos(s, newline_indices, newline_indices[i]);
            unsigned long line_start_idx = startend.first;
            unsigned long line_end_idx = startend.second;
            lines.insert(lines.end(), s.substr(line_start_idx, line_end_idx - line_start_idx));
            ++i;
            --line_count;
        }
        return lines;
    }

    std::vector<FindResult*> Finder::find_multiline_string(std::string& s) {
        std::vector<FindResult*> results = {};

        // get newline, startline and endline indices
        std::vector<unsigned long> newline_indices = get_newline_indices(s);
        std::vector<unsigned long> line_start_indices = {0};
        auto plus_one = [](unsigned long num) {return num + 1;};
        std::transform(newline_indices.begin(), newline_indices.end(), std::back_inserter(line_start_indices), plus_one);

        for (const auto& p : *(m_settings->findpatterns())) {
            // ---------------------------------------------------------------------
            auto matches_begin = std::sregex_iterator(s.begin(), s.end(), p->r());
            auto matches_end = std::sregex_iterator();

            for (std::sregex_iterator it = matches_begin; it != matches_end; ++it) {

                std::smatch match = *it;

                unsigned long match_start_idx = match.position(0);
                unsigned long match_end_idx = match_start_idx + match.length(0);

                unsigned long linenum = get_linenum_for_pos(newline_indices, match_start_idx);
                std::pair<unsigned long, unsigned long> startend =
                        get_linestartend_for_pos(s, newline_indices, match_start_idx);
                unsigned long line_start_idx = startend.first;
                unsigned long line_end_idx = startend.second;

                std::string line = s.substr(line_start_idx, line_end_idx - line_start_idx);

                std::vector<std::string> lines_before;
                if (m_settings->linesbefore() > 0) {
                    lines_before = get_lines_before_pos(s, newline_indices, m_settings->linesbefore(),
                                                        line_start_idx);
                    if (!lines_match(lines_before, m_settings->in_linesbeforepatterns(),
                                     m_settings->out_linesbeforepatterns())) {
                        continue;
                    }
                }
                std::vector<std::string> lines_after;
                if (m_settings->linesafter() > 0) {
                    lines_after = get_lines_after_pos(s, newline_indices, m_settings->linesafter(),
                                                       line_start_idx);
                    if (!lines_match(lines_after, m_settings->in_linesafterpatterns(),
                                     m_settings->out_linesafterpatterns())) {
                        continue;
                    }
                }

                results.push_back(new FindResult(p, nullptr, linenum,
                                                   match_start_idx - line_start_idx + 1,
                                                   match_end_idx - line_start_idx + 1,
                                                   line, &lines_before, &lines_after));

                if (m_settings->firstmatch()) {
                    break;
                }
            }
            // ---------------------------------------------------------------------
        }

        return results;
    }

    std::vector<FindResult*> Finder::find_binary_file(FindFile* sf) {
        std::vector<FindResult*> results = {};
        std::set <std::string> found_patterns = {};

        std::ifstream fin(sf->string());
        std::string s = FileUtil::get_contents(fin);
        fin.close();

        std::smatch pmatch;
        for (const auto& p : *(m_settings->findpatterns())) {
            if (m_settings->firstmatch() && found_patterns.find(p->pattern()) != found_patterns.end()) {
                continue;
            }
            unsigned long trimmed = 0;
            std::string sbuf = std::string(s);
            bool skip_pattern = false;
            while (!skip_pattern && !sbuf.empty() && regex_find(sbuf, pmatch, p->r())) {
                for (unsigned i=0; i < pmatch.size(); ++i) {
                    unsigned long match_start_idx = pmatch.position(i) + trimmed;
                    unsigned long match_end_idx = match_start_idx + pmatch.length(i);
                    results.push_back(new FindResult(p, sf, 0, match_start_idx, match_end_idx, ""));
                    if (m_settings->firstmatch()) {
                        found_patterns.insert(p->pattern());
                        skip_pattern = true;
                        break;
                    }
                    sbuf = s.substr(match_end_idx);
                    trimmed = match_end_idx;
                }
            }
        }

        return results;
    }
}
