#include <algorithm>
#include <string>

#include "rapidjson/document.h"
#include "rapidjson/filereadstream.h"

#include "config.h"
#include "FileTypes.h"
#include "FileUtil.h"
#include "FindException.h"
#include "StringUtil.h"


namespace cppfind {
    FileTypes::FileTypes() {
        load_file_types();
    }

    void FileTypes::load_file_types() {
        auto file_types_path = FileUtil::join_path(xfindpath(), "shared/filetypes.json");

        if (!FileUtil::file_exists(file_types_path)) {
            throw FindException("Filetypes file not found: " + file_types_path);
        }

        uint64_t file_size = FileUtil::file_size(file_types_path);
        FILE* fp = fopen(file_types_path.c_str(), "r");

        char readBuffer[file_size];
        rapidjson::FileReadStream is(fp, readBuffer, sizeof(readBuffer));

        rapidjson::Document document;

        document.ParseStream(is);
        fclose(fp);

        assert(document.HasMember("filetypes"));
        const rapidjson::Value& filetypes = document["filetypes"];
        assert(filetypes.IsArray());
        for (rapidjson::SizeType i = 0; i < filetypes.Size(); i++) {
            const rapidjson::Value::ConstObject &filetype = filetypes[i].GetObject();
            assert(filetype.HasMember("type"));
            const rapidjson::Value &typeValue = filetype["type"];
            std::string type = typeValue.GetString();

            assert(filetype.HasMember("extensions"));
            const rapidjson::Value& extensions = filetype["extensions"];

            for (rapidjson::SizeType j = 0; j < extensions.Size(); j++) {
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
            const rapidjson::Value& names = filetype["names"];

            for (rapidjson::SizeType j = 0; j < names.Size(); j++) {
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
         std::string uname{name};
        std::transform(uname.begin(), uname.end(), uname.begin(),
                       [](unsigned char c) { return std::toupper(c); });
        if (uname == "ARCHIVE") {
            return FileType::ARCHIVE;
        }
        else if (uname == "BINARY") {
            return FileType::BINARY;
        }
        else if (uname == "CODE") {
            return FileType::CODE;
        }
        else if (uname == "TEXT") {
            return FileType::TEXT;
        }
        else if (uname == "XML") {
            return FileType::XML;
        }
        else {
            return FileType::UNKNOWN;
        }
    }

    std::string FileTypes::to_name(const FileType& file_type) {
        switch (file_type) {
            case FileType::ARCHIVE:
                return "archive";
            case FileType::BINARY:
                return "binary";
            case FileType::CODE:
                return "code";
            case FileType::TEXT:
                return "text";
            case FileType::XML:
                return "xml";
            default:
                return "unknown";
        }
    }

    FileType FileTypes::get_file_type(const std::string &file_path) {
        if (is_code_file(file_path)) {
            return FileType::CODE;
        }
        if (is_xml_file(file_path)) {
            return FileType::XML;
        }
        if (is_text_file(file_path)) {
            return FileType::TEXT;
        }
        if (is_binary_file(file_path)) {
            return FileType::BINARY;
        }
        if (is_archive_file(file_path)) {
            return FileType::ARCHIVE;
        }
        return FileType::UNKNOWN;
    }

    bool FileTypes::is_archive_file(const std::string &file_path) {
        return StringUtil::string_in_set(FileUtil::get_extension(file_path), m_archive_extensions)
               || StringUtil::string_in_set(FileUtil::get_file_name(file_path), m_archive_names);
    }

    bool FileTypes::is_binary_file(const std::string &file_path) {
        return StringUtil::string_in_set(FileUtil::get_extension(file_path), m_binary_extensions)
               || StringUtil::string_in_set(FileUtil::get_file_name(file_path), m_binary_names);
    }

    bool FileTypes::is_code_file(const std::string &file_path) {
        return StringUtil::string_in_set(FileUtil::get_extension(file_path), m_code_extensions)
               || StringUtil::string_in_set(FileUtil::get_file_name(file_path), m_code_names);
    }

    bool FileTypes::is_text_file(const std::string &file_path) {
        return StringUtil::string_in_set(FileUtil::get_extension(file_path), m_text_extensions)
               || StringUtil::string_in_set(FileUtil::get_file_name(file_path), m_text_names);
    }

    bool FileTypes::is_unknown_file(const std::string &file_path) {
        return get_file_type(file_path) == FileType::UNKNOWN;
    }

    bool FileTypes::is_xml_file(const std::string &file_path) {
        return StringUtil::string_in_set(FileUtil::get_extension(file_path), m_xml_extensions)
               || StringUtil::string_in_set(FileUtil::get_file_name(file_path), m_xml_names);
    }

    FileTypes::~FileTypes() {

    }
}
