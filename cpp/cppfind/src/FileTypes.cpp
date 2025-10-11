#include <algorithm>
#include <string>

#include "rapidjson/document.h"
#include "rapidjson/filereadstream.h"

#include "FindConfig.h"
#include "FileTypes.h"
#include "FileUtil.h"
#include "FindException.h"
#include "StringUtil.h"


namespace cppfind {
    FileTypes::FileTypes() {
        load_file_types();
    }

    void FileTypes::load_file_types() {
        auto file_types_path = std::filesystem::path(xfindpath()) / "shared/filetypes.json";

        if (!std::filesystem::exists(file_types_path)) {
            throw FindException("Filetypes file not found: " + file_types_path.string());
        }

        uint64_t file_size = std::filesystem::file_size(file_types_path);
        // current size is 12076, make sure it's not dramatically bigger than that
        if (file_size > 12100) {
            throw FindException("Invalid filetypes file");
        }

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
                if (type == FILE_TYPE_NAME_ARCHIVE) {
                    m_archive_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_AUDIO) {
                    m_audio_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_BINARY) {
                    m_binary_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_CODE) {
                    m_code_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_FONT) {
                    m_font_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_IMAGE) {
                    m_image_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_TEXT) {
                    m_text_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_VIDEO) {
                    m_video_extensions.insert(extensions[j].GetString());
                } else if (type == FILE_TYPE_NAME_XML) {
                    m_xml_extensions.insert(extensions[j].GetString());
                }
            }

            assert(filetype.HasMember("names"));
            const rapidjson::Value& names = filetype["names"];

            for (rapidjson::SizeType j = 0; j < names.Size(); j++) {
                if (type == FILE_TYPE_NAME_ARCHIVE) {
                    m_archive_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_AUDIO) {
                    m_audio_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_BINARY) {
                    m_binary_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_CODE) {
                    m_code_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_FONT) {
                    m_font_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_IMAGE) {
                    m_image_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_TEXT) {
                    m_text_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_VIDEO) {
                    m_video_names.insert(names[j].GetString());
                } else if (type == FILE_TYPE_NAME_XML) {
                    m_xml_names.insert(names[j].GetString());
                }
            }
        }
    }

    FileType FileTypes::from_name(const std::string_view name) {
         std::string lname{name};
        std::ranges::transform(lname.begin(), lname.end(), lname.begin(),
                       [](const unsigned char c) { return std::tolower(c); });
        if (lname == FILE_TYPE_NAME_ARCHIVE) {
            return FileType::ARCHIVE;
        }
        if (lname == FILE_TYPE_NAME_AUDIO) {
            return FileType::AUDIO;
        }
        if (lname == FILE_TYPE_NAME_BINARY) {
            return FileType::BINARY;
        }
        if (lname == FILE_TYPE_NAME_CODE) {
            return FileType::CODE;
        }
        if (lname == FILE_TYPE_NAME_FONT) {
            return FileType::FONT;
        }
        if (lname == FILE_TYPE_NAME_IMAGE) {
            return FileType::IMAGE;
        }
        if (lname == FILE_TYPE_NAME_TEXT) {
            return FileType::TEXT;
        }
        if (lname == FILE_TYPE_NAME_VIDEO) {
            return FileType::VIDEO;
        }
        if (lname == FILE_TYPE_NAME_XML) {
            return FileType::XML;
        }
        return FileType::UNKNOWN;
    }

    std::string FileTypes::to_name(const FileType& file_type) {
        switch (file_type) {
            case FileType::ARCHIVE:
                return FILE_TYPE_NAME_ARCHIVE;
            case FileType::AUDIO:
                return FILE_TYPE_NAME_AUDIO;
            case FileType::BINARY:
                return FILE_TYPE_NAME_BINARY;
            case FileType::CODE:
                return FILE_TYPE_NAME_CODE;
            case FileType::FONT:
                return FILE_TYPE_NAME_FONT;
            case FileType::IMAGE:
                return FILE_TYPE_NAME_IMAGE;
            case FileType::TEXT:
                return FILE_TYPE_NAME_TEXT;
            case FileType::VIDEO:
                return FILE_TYPE_NAME_VIDEO;
            case FileType::XML:
                return FILE_TYPE_NAME_XML;
            default:
                return "unknown";
        }
    }

    FileType FileTypes::get_path_type(const std::filesystem::path& file_path) const {
        // most specific first
        if (is_code_path(file_path)) {
            return FileType::CODE;
        }
        if (is_archive_path(file_path)) {
            return FileType::ARCHIVE;
        }
        if (is_audio_path(file_path)) {
            return FileType::AUDIO;
        }
        if (is_font_path(file_path)) {
            return FileType::FONT;
        }
        if (is_image_path(file_path)) {
            return FileType::IMAGE;
        }
        if (is_video_path(file_path)) {
            return FileType::VIDEO;
        }
        // most general last
        if (is_xml_path(file_path)) {
            return FileType::XML;
        }
        if (is_text_path(file_path)) {
            return FileType::TEXT;
        }
        if (is_binary_path(file_path)) {
            return FileType::BINARY;
        }
        return FileType::UNKNOWN;
    }

    bool is_path_with_extension_or_name(const std::filesystem::path& path,
        const std::unordered_set<std::string>& extensions, const std::unordered_set<std::string>& filenames) {
        const auto file_ext = FileUtil::get_path_extension(path);
        if (!StringUtil::string_in_unordered_set(file_ext, extensions)) {
            const auto filename = path.filename();
            return !filename.empty() && StringUtil::string_in_unordered_set(filename.string(), filenames);
        }
        return true;
    }

    bool FileTypes::is_archive_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_archive_extensions, m_archive_names);
    }

    bool FileTypes::is_audio_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_audio_extensions, m_audio_names);
    }

    bool FileTypes::is_binary_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_binary_extensions, m_binary_names);
    }

    bool FileTypes::is_code_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_code_extensions, m_code_names);
    }

    bool FileTypes::is_font_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_font_extensions, m_font_names);
    }

    bool FileTypes::is_image_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_image_extensions, m_image_names);
    }

    bool FileTypes::is_text_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_text_extensions, m_text_names);
    }

    bool FileTypes::is_video_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_video_extensions, m_video_names);
    }

    bool FileTypes::is_unknown_path(const std::filesystem::path& file_path) const {
        return get_path_type(file_path) == FileType::UNKNOWN;
    }

    bool FileTypes::is_xml_path(const std::filesystem::path& file_path) const {
        return is_path_with_extension_or_name(file_path, m_xml_extensions, m_xml_names);
    }
}
