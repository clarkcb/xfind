#include <boost/algorithm/string.hpp>
#include "rapidjson/document.h"
#include "rapidjson/filereadstream.h"
#include "common.h"
#include "config.h"
#include "FindException.h"
#include "FileTypes.h"
#include "FileUtil.h"

using namespace rapidjson;

namespace cppfind {
    FileTypes::FileTypes() {
        m_archive_extensions = {};
        m_archive_names = {};
        m_binary_extensions = {};
        m_binary_names = {};
        m_code_extensions = {};
        m_code_names = {};
        m_text_extensions = {};
        m_text_names = {};
        m_xml_extensions = {};
        m_xml_names = {};
        load_filetypes();
    }

    void FileTypes::load_filetypes() {
        auto xfind_path = xfindpath();
        auto sub_path = "shared/filetypes.json";
        auto filetypes_path = FileUtil::join_path(xfind_path, sub_path);

        if (!FileUtil::file_exists(filetypes_path)) {
            std::string msg = "Filetypes file not found: ";
            msg.append(filetypes_path);
            throw FindException(msg);
        }

        uint64_t file_size = FileUtil::file_size(filetypes_path);
        FILE* fp = fopen(filetypes_path.c_str(), "r");

        char readBuffer[file_size];
        FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("filetypes"));
        const Value& filetypes = document["filetypes"];
        assert(filetypes.IsArray());
        for (SizeType i = 0; i < filetypes.Size(); i++) {
            const Value::ConstObject &filetype = filetypes[i].GetObject();
            assert(filetype.HasMember("type"));
            const Value &typeValue = filetype["type"];
            std::string type = typeValue.GetString();

            assert(filetype.HasMember("extensions"));
            const Value& extensions = filetype["extensions"];

            for (SizeType j = 0; j < extensions.Size(); j++) {
                if (type == "archive") {
                    m_archive_extensions.insert(extensions[j].GetString());
                } else if (type == "binary") {
                    m_binary_extensions.insert(extensions[j].GetString());
                } else if (type == "code") {
                    m_code_extensions.insert(extensions[j].GetString());
                } else if (type == "text") {
                    m_text_extensions.insert(extensions[j].GetString());
                } else if (type == "xml") {
                    m_xml_extensions.insert(extensions[j].GetString());
                }
            }

            assert(filetype.HasMember("names"));
            const Value& names = filetype["names"];

            for (SizeType j = 0; j < names.Size(); j++) {
                if (type == "archive") {
                    m_archive_names.insert(names[j].GetString());
                } else if (type == "binary") {
                    m_binary_names.insert(names[j].GetString());
                } else if (type == "code") {
                    m_code_names.insert(names[j].GetString());
                } else if (type == "text") {
                    m_text_names.insert(names[j].GetString());
                } else if (type == "xml") {
                    m_xml_names.insert(names[j].GetString());
                }
            }
        }
    }

    FileType FileTypes::from_name(const std::string& name) {
        std::string uname = boost::to_upper_copy(name);
        if (uname == "TEXT") {
            return FileType::TEXT;
        }
        if (uname == "BINARY") {
            return FileType::BINARY;
        }
        if (uname == "CODE") {
            return FileType::CODE;
        }
        if (uname == "XML") {
            return FileType::XML;
        }
        if (uname == "ARCHIVE") {
            return FileType::ARCHIVE;
        }
        return FileType::UNKNOWN;
    }

    std::string FileTypes::to_name(const FileType filetype) {
        switch (filetype)
        {
        case FileType::ARCHIVE:
            return "ARCHIVE";
        case FileType::CODE:
            return "CODE";
        case FileType::BINARY:
            return "BINARY";
        case FileType::TEXT:
            return "TEXT";
        case FileType::XML:
            return "XML";
        default:
            return "UNKNOWN";
        }
    }

    FileType FileTypes::get_filetype(const std::string& filepath) {
        if (is_code_file(filepath)) {
            return FileType::CODE;
        }
        if (is_xml_file(filepath)) {
            return FileType::XML;
        }
        if (is_text_file(filepath)) {
            return FileType::TEXT;
        }
        if (is_binary_file(filepath)) {
            return FileType::BINARY;
        }
        if (is_archive_file(filepath)) {
            return FileType::ARCHIVE;
        }
        return FileType::UNKNOWN;
    }

    bool FileTypes::string_in_set(const std::set<std::string>* set, const std::string& s) {
        auto found = set->find(s);
        return found != set->end();
    }

    bool FileTypes::is_archive_file(const std::string& filepath) {
        return string_in_set(&m_archive_extensions, FileUtil::get_extension(filepath))
               || string_in_set(&m_archive_names, FileUtil::get_filename(filepath));
    }

    bool FileTypes::is_binary_file(const std::string& filepath) {
        return string_in_set(&m_binary_extensions, FileUtil::get_extension(filepath))
               || string_in_set(&m_binary_names, FileUtil::get_filename(filepath));
    }

    bool FileTypes::is_code_file(const std::string& filepath) {
        return string_in_set(&m_code_extensions, FileUtil::get_extension(filepath))
               || string_in_set(&m_code_names, FileUtil::get_filename(filepath));
    }

    bool FileTypes::is_text_file(const std::string& filepath) {
        std::string ext = FileUtil::get_extension(filepath);
        std::string filename = FileUtil::get_filename(filepath);
        return string_in_set(&m_text_extensions, ext)
               || string_in_set(&m_text_names, filename)
               || string_in_set(&m_code_extensions, ext)
               || string_in_set(&m_code_names, filename)
               || string_in_set(&m_xml_extensions, ext)
               || string_in_set(&m_xml_names, filename);
    }

    bool FileTypes::is_unknown_file(const std::string& filepath) {
        return get_filetype(filepath) == FileType::UNKNOWN;
    }

    bool FileTypes::is_xml_file(const std::string& filepath) {
        return string_in_set(&m_xml_extensions, FileUtil::get_extension(filepath))
               || string_in_set(&m_xml_names, FileUtil::get_filename(filepath));
    }
}
