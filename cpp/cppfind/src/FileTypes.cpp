#include <algorithm>
#include <string>

#include "FindConfig.h"
#include "FileTypes.h"
#include "FileUtil.h"
#include "FindException.h"
#include "StringUtil.h"


namespace cppfind {
    FileTypes::FileTypes() {
        auto xfind_db_path = std::filesystem::path(xfindpath()) / "shared/xfind.db";
        sqlite3 *db;
        if (const int rc = sqlite3_open_v2(xfind_db_path.string().c_str(), &db, SQLITE_OPEN_READONLY, nullptr);
            rc == SQLITE_OK && db != nullptr) {
            this->db = db;
        } else {
            throw FindException(sqlite3_errmsg(db));
        }
        load_name_type_cache();
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

    std::unordered_map<std::string, FileType> FileTypes::get_file_types_for_query_and_params(
        const std::string_view query,
        const std::vector<std::string_view>& params) const {
        std::unordered_map<std::string, FileType> file_type_map{};
        sqlite3_stmt *stmt;
        sqlite3_prepare_v2(this->db, query.data(), -1, &stmt, nullptr);
        for (int i = 0; i < params.size(); i++) {
            if (const int rc = sqlite3_bind_text(stmt, i + 1, params[i].data(), -1, SQLITE_TRANSIENT);
                rc != SQLITE_OK) {
                fprintf(stderr, "error: %s\n", sqlite3_errmsg(this->db));
                }
        }
        while (sqlite3_step(stmt) == SQLITE_ROW) {
            const char *key  = (char *)sqlite3_column_text(stmt, 0);
            const int file_type_id  = sqlite3_column_int(stmt, 1) - 1;
            file_type_map.insert({std::string(key), static_cast<FileType>(file_type_id)});
        }
        return file_type_map;
    }

    void FileTypes::load_name_type_cache() {
        const auto query = "select name, file_type_id from file_name";
        std::vector<std::string_view> params{};
        name_type_cache = get_file_types_for_query_and_params(query, params);
        name_type_cache_loaded = true;
    }

    FileType FileTypes::get_file_type_for_query_and_params(const std::string_view query,
                                                           const std::vector<std::string_view>& params) const {
        sqlite3_stmt *stmt;
        sqlite3_prepare_v2(this->db, query.data(), -1, &stmt, nullptr);
        for (int i = 0; i < params.size(); i++) {
            if (const int rc = sqlite3_bind_text(stmt, i + 1, params[i].data(), -1, SQLITE_TRANSIENT);
                rc != SQLITE_OK) {
                fprintf(stderr, "error: %s\n", sqlite3_errmsg(this->db));
            }
        }
        if (sqlite3_step(stmt) == SQLITE_ROW) {
            const int file_type_id  = sqlite3_column_int(stmt, 0) - 1;
            return static_cast<FileType>(file_type_id);
        }
        return FileType::UNKNOWN;
    }

    FileType FileTypes::get_file_type_for_file_name(const std::string_view file_name) {
        // if (!name_type_cache_loaded) {
        //     load_name_type_cache();
        // }
        const std::string s_file_name{file_name};
        if (name_type_cache.contains(s_file_name)) {
            return name_type_cache[s_file_name];
        }
        // const auto query = "select file_type_id from file_name where name=?";
        // std::vector params{file_name};
        // return get_file_type_for_query_and_params(query, params);
        return FileType::UNKNOWN;
    }

    FileType FileTypes::get_file_type_for_extension(const std::string_view file_ext) {
        if (file_ext.empty()) {
            return FileType::UNKNOWN;
        }
        const std::string s_file_ext{file_ext};
        if (ext_type_cache.contains(s_file_ext)) {
            return ext_type_cache[s_file_ext];
        }
        const auto query = "select file_type_id from file_extension where extension=?";
        std::vector params{file_ext};
        FileType file_type = get_file_type_for_query_and_params(query, params);
        ext_type_cache.insert({s_file_ext, file_type});
        return file_type;
    }

    FileType FileTypes::get_file_type_for_path(const std::filesystem::path& file_path) {
        if (const FileType file_type = get_file_type_for_file_name(file_path.filename().string());
            file_type != FileType::UNKNOWN) {
            return file_type;
        }
        return get_file_type_for_extension(FileUtil::get_path_extension(file_path));
    }

    bool FileTypes::is_archive_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::ARCHIVE;
    }

    bool FileTypes::is_audio_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::AUDIO;
    }

    bool FileTypes::is_binary_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::BINARY;
    }

    bool FileTypes::is_code_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::CODE;
    }

    bool FileTypes::is_font_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::FONT;
    }

    bool FileTypes::is_image_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::IMAGE;
    }

    bool FileTypes::is_text_path(const std::filesystem::path& file_path) {
        const FileType file_type = get_file_type_for_path(file_path);
        return file_type == FileType::TEXT
            || file_type == FileType::CODE
            || file_type == FileType::XML;
    }

    bool FileTypes::is_video_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::VIDEO;
    }

    bool FileTypes::is_unknown_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::UNKNOWN;
    }

    bool FileTypes::is_xml_path(const std::filesystem::path& file_path) {
        return get_file_type_for_path(file_path) == FileType::XML;
    }
}
