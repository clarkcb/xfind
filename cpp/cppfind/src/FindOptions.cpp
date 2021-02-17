#include <algorithm>
#include <cstdlib>
#include <deque>
#include <iostream>
#include <boost/format.hpp>
#include "rapidjson/filereadstream.h"
#include "config.h"
#include "FileTypes.h"
#include "FileUtil.h"
#include "FindException.h"
#include "FindOption.h"
#include "FindOptions.h"

namespace cppfind {
    FindOptions::FindOptions() {
        m_coll_arg_map = {
                {"in-archiveext", [](std::string& s, FindSettings* ss) { ss->add_in_archiveextension(s); }},
                {"in-archivefilepattern", [](std::string& s, FindSettings* ss) { ss->add_in_archivefilepattern(s); }},
                {"in-dirpattern", [](std::string& s, FindSettings* ss) { ss->add_in_dirpattern(s); }},
                {"in-ext", [](std::string& s, FindSettings* ss) { ss->add_in_extension(s); }},
                {"in-filepattern", [](std::string& s, FindSettings* ss) { ss->add_in_filepattern(s); }},
                {"in-filetype", [](std::string& s, FindSettings* ss) { auto t = FileTypes::from_name(s); ss->add_in_filetype(t); }},
                {"in-linesafterpattern", [](std::string& s, FindSettings* ss) { ss->add_in_linesafterpattern(s); }},
                {"in-linesbeforepattern", [](std::string& s, FindSettings* ss) { ss->add_in_linesbeforepattern(s); }},
                {"linesaftertopattern", [](std::string& s, FindSettings* ss) { ss->add_linesaftertopattern(s); }},
                {"linesafteruntilpattern", [](std::string& s, FindSettings* ss) { ss->add_linesafteruntilpattern(s); }},
                {"out-archiveext", [](std::string& s, FindSettings* ss) { ss->add_out_archiveextension(s); }},
                {"out-archivefilepattern", [](std::string& s, FindSettings* ss) { ss->add_out_archivefilepattern(s); }},
                {"out-dirpattern", [](std::string& s, FindSettings* ss) { ss->add_out_dirpattern(s); }},
                {"out-ext", [](std::string& s, FindSettings* ss) { ss->add_out_extension(s); }},
                {"out-filepattern", [](std::string& s, FindSettings* ss) { ss->add_out_filepattern(s); }},
                {"out-filetype", [](std::string& s, FindSettings* ss) { auto t = FileTypes::from_name(s); ss->add_out_filetype(t); }},
                {"out-linesafterpattern", [](std::string& s, FindSettings* ss) { ss->add_out_linesafterpattern(s); }},
                {"out-linesbeforepattern", [](std::string& s, FindSettings* ss) { ss->add_out_linesbeforepattern(s); }},
                {"findpattern", [](std::string& s, FindSettings* ss) { ss->add_findpattern(s); }},
                {"settings-file", [this](std::string& s, FindSettings* ss) { this->settings_from_file(s, ss); }}
        };

        m_int_arg_map = {
                {"linesafter", [](unsigned int i, FindSettings* ss) { ss->linesafter(i); }},
                {"linesbefore", [](unsigned int i, FindSettings* ss) { ss->linesbefore(i); }},
                {"maxlinelength", [](unsigned int i, FindSettings* ss) { ss->maxlinelength(i); }}
        };

        m_str_arg_map = {
                {"startpath", [](std::string& s, FindSettings* ss) { ss->startpath(s); }},
                {"settings-file", [this](std::string& s, FindSettings* ss) { this->settings_from_file(s, ss); }}
        };

        m_bool_arg_map = {
                {"archivesonly", [](bool b, FindSettings* ss) { ss->archivesonly(b); }},
                {"allmatches", [](bool b, FindSettings* ss) { ss->firstmatch(!b); }},
                {"colorize", [](bool b, FindSettings* ss) { ss->colorize(b); }},
                {"debug", [](bool b, FindSettings* ss) { ss->debug(b); }},
                {"excludehidden", [](bool b, FindSettings* ss) { ss->excludehidden(b); }},
                {"firstmatch", [](bool b, FindSettings* ss) { ss->firstmatch(b); }},
                {"help", [](bool b, FindSettings* ss) { ss->printusage(b); }},
                {"includehidden", [](bool b, FindSettings* ss) { ss->excludehidden(!b); }},
                {"listdirs", [](bool b, FindSettings* ss) { ss->listdirs(b); }},
                {"listfiles", [](bool b, FindSettings* ss) { ss->listfiles(b); }},
                {"listlines", [](bool b, FindSettings* ss) { ss->listlines(b); }},
                {"multilineoption-REMOVE", [](bool b, FindSettings* ss) { ss->multilineoption-REMOVE(b); }},
                {"nocolorize", [](bool b, FindSettings* ss) { ss->colorize(!b); }},
                {"noprintmatches", [](bool b, FindSettings* ss) { ss->printresults(!b); }},
                {"norecursive", [](bool b, FindSettings* ss) { ss->recursive(!b); }},
                {"nofindarchives", [](bool b, FindSettings* ss) { ss->findarchives(!b); }},
                {"printmatches", [](bool b, FindSettings* ss) { ss->printresults(b); }},
                {"recursive", [](bool b, FindSettings* ss) { ss->recursive(b); }},
                {"findarchives", [](bool b, FindSettings* ss) { ss->findarchives(b); }},
                {"uniquelines", [](bool b, FindSettings* ss) { ss->uniquelines(b); }},
                {"verbose", [](bool b, FindSettings* ss) { ss->verbose(b); }},
                {"version", [](bool b, FindSettings* ss) { ss->printversion(b); }},
        };

        m_long_arg_map = {};
        m_options = {};
        load_options();
    }

    void FindOptions::settings_from_file(std::string& filepath, FindSettings* settings) {
        if (!FileUtil::file_exists(filepath)) {
            std::string msg = "Settings file not found: ";
            msg.append(filepath);
            throw FindException(msg);
        }

        FILE *fp = fopen(filepath.c_str(), "r");

        char readBuffer[65536];
        FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        Document document;
        document.ParseStream(is);
        fclose(fp);

        settings_from_document(&document, settings);
    }

    void FindOptions::settings_from_json(std::string& json, FindSettings* settings) {
        Document document;
        document.Parse(json.c_str());
        settings_from_document(&document, settings);
    }

    void FindOptions::settings_from_document(Document* document, FindSettings* settings) {
        assert(document->IsObject());

        for(Value::ConstMemberIterator it=document->MemberBegin(); it != document->MemberEnd(); it++) {
            std::string name = it->name.GetString();

            if (it->value.IsArray()) {
                assert(m_coll_arg_map.find(name) != m_coll_arg_map.end());
                const auto& arr = it->value.GetArray();
                for (SizeType i = 0; i < arr.Size(); i++) {
                    assert(arr[i].IsString());
                    auto* s = new std::string(arr[i].GetString());
                    m_coll_arg_map[name](*s, settings);
                }

            } else if (it->value.IsNumber()) {
                assert(m_int_arg_map.find(name) != m_int_arg_map.end());
                int i = it->value.GetInt();
                if (i < 0) {
                    std::string msg = "Invalid value for option ";
                    msg.append(name).append(": ").append(std::to_string(i));
                    throw FindException(msg);
                }
                m_int_arg_map[name]((unsigned int)i, settings);

            } else if (it->value.IsBool()) {
                assert(m_bool_arg_map.find(name) != m_bool_arg_map.end());
                bool b = it->value.GetBool();
                m_bool_arg_map[name](b, settings);

            } else if (it->value.IsString()) {
                auto* s = new std::string(it->value.GetString());
                if (m_str_arg_map.find(name) != m_str_arg_map.end()) {
                    m_str_arg_map[name](*s, settings);
                } else if (m_coll_arg_map.find(name) != m_coll_arg_map.end()) {
                    m_coll_arg_map[name](*s, settings);
                } else {
                    std::string msg = "Invalid option: " + name;
                    throw FindException(msg);
                }
            }
        }
    }

    void FindOptions::load_options() {
        auto findoptions_path = std::string(XFINDPATH);
        findoptions_path.append("/shared/findoptions.json");

        if (!FileUtil::file_exists(findoptions_path)) {
            std::string msg = "Findoptions file not found: ";
            msg.append(findoptions_path);
            throw FindException(msg);
        }

        FILE* fp = fopen(findoptions_path.c_str(), "r");

        char readBuffer[65536];
        FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("findoptions"));
        const Value& findoptions = document["findoptions"];
        assert(findoptions.IsArray());
        for (SizeType i = 0; i < findoptions.Size(); i++) {
            const Value::ConstObject &findoption = findoptions[i].GetObject();
            assert(findoption.HasMember("long"));
            const Value &longValue = findoption["long"];
            auto* lng = new std::string(longValue.GetString());
            m_long_arg_map[*lng] = *lng;

            std::string* sht;
            if (findoption.HasMember("short")) {
                const Value &shortValue = findoption["short"];
                sht = new std::string(shortValue.GetString());
                m_long_arg_map[*sht] = *lng;
            } else {
                sht = new std::string("");
            }

            assert(findoption.HasMember("desc"));
            const Value &descValue = findoption["desc"];
            auto* desc = new std::string(descValue.GetString());

            auto* option = new FindOption(sht, *lng, *desc);
            m_options.push_back(option);
        }
    }

    FindSettings* FindOptions::settings_from_args(int &argc, char **argv) {
        auto *settings = new FindSettings();

        // set print results to true since we are running the executable
        settings->printresults(true);

        std::deque<std::string> arg_deque;
        unsigned int i;

        for (i=1; i < argc; i++) {
            auto arg = new std::string(argv[i]);
            arg_deque.push_back(*arg);
        }

        std::string* next_arg;
        while (!arg_deque.empty()) {
            next_arg = new std::string(arg_deque.front());
            arg_deque.pop_front();

            if ((*next_arg)[0] == '-') {
                while (!next_arg->empty() && (*next_arg)[0] == '-') {
                    *next_arg = next_arg->substr(1);
                }

                auto long_arg_found = m_long_arg_map.find(*next_arg);
                if (long_arg_found != m_long_arg_map.end()) {
                    auto longarg = m_long_arg_map[*next_arg];

                    auto bool_arg_found = m_bool_arg_map.find(longarg);
                    auto coll_arg_found = m_coll_arg_map.find(longarg);
                    auto int_arg_found = m_int_arg_map.find(longarg);
                    auto str_arg_found = m_str_arg_map.find(longarg);

                    if (bool_arg_found != m_bool_arg_map.end()) {
                        m_bool_arg_map[longarg](true, settings);
                    } else if (coll_arg_found != m_coll_arg_map.end()
                               || int_arg_found != m_int_arg_map.end()
                               || str_arg_found != m_str_arg_map.end()) {
                        if (arg_deque.empty()) {
                            std::string msg = "Missing value for option ";
                            msg.append(*next_arg);
                            throw FindException(msg);
                        } else {
                            auto* arg_val = new std::string(arg_deque.front());
                            arg_deque.pop_front();
                            if (coll_arg_found != m_coll_arg_map.end()) {
                                m_coll_arg_map[longarg](*arg_val, settings);
                            } else if (int_arg_found != m_int_arg_map.end()) {
                                int int_val = stoi(*arg_val);
                                if (int_val < 0) {
                                    std::string msg = "Invalid value for option ";
                                    msg.append(*next_arg).append(": ").append(*arg_val);
                                    throw FindException(msg);
                                }
                                m_int_arg_map[longarg](int_val, settings);
                            } else if (str_arg_found != m_str_arg_map.end()) {
                                m_str_arg_map[longarg](*arg_val, settings);
                            }
                        }
                    } else { // shouldn't be possible to get here
                        std::string msg = "Invalid option: ";
                        msg.append(*next_arg);
                        throw FindException(msg);
                    }
                } else {
                    std::string msg = "Invalid option: ";
                    msg.append(*next_arg);
                    throw FindException(msg);
                }
            } else {
                settings->startpath(*next_arg);
            }
        }
        return settings;
    }

    void FindOptions::usage() {
        std::string usage_string = get_usage_string();
        std::cout << usage_string << std::endl;
        exit(1);
    }

    std::string FindOptions::get_usage_string() {
        auto* usage_string = new std::string("\nUsage:\n cppfind [options] -s <findpattern> <startpath>\n\nOptions:\n");

        std::vector<std::string> opt_strings = {};
        std::vector<std::string> opt_descs = {};

        auto sort_option_lambda = [](const FindOption* s1, const FindOption* s2) -> bool {
            return s1->sortarg().compare(s2->sortarg()) < 0;
        };
        sort(m_options.begin(), m_options.end(), sort_option_lambda);

        long longest = 0;
        for (auto const& option : m_options) {
            std::string opt_string{};
            const std::string* shortarg = option->shortarg();
            if (!shortarg->empty()) {
                opt_string.append("-").append(*shortarg).append(",");
            }
//            const std::string longarg = option->longarg();
            opt_string.append("--").append(option->longarg());
            if (opt_string.length() > longest) {
                longest = opt_string.length();
            }
            opt_strings.push_back(opt_string);
//            const std::string description = option->description();
            opt_descs.push_back(option->description());
        }

        std::string format = std::string(" %1$-") + std::to_string(longest) + "s  %2$s\n";
        for (int i = 0; i < opt_strings.size(); i++) {
            usage_string->append(boost::str(boost::format(format) % opt_strings[i] % opt_descs[i]));
        }
        return *usage_string;
    }
}
