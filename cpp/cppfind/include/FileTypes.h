#ifndef CPPFIND_FILETYPES_H
#define CPPFIND_FILETYPES_H

#include <filesystem>
#include <sqlite3.h>
#include <unordered_map>

#define FILE_TYPE_NAME_ARCHIVE "archive"
#define FILE_TYPE_NAME_AUDIO "audio"
#define FILE_TYPE_NAME_BINARY "binary"
#define FILE_TYPE_NAME_CODE "code"
#define FILE_TYPE_NAME_FONT "font"
#define FILE_TYPE_NAME_IMAGE "image"
#define FILE_TYPE_NAME_TEXT "text"
#define FILE_TYPE_NAME_VIDEO "video"
#define FILE_TYPE_NAME_XML "xml"
#define FILE_TYPE_NAME_NOSEARCH "nosearch"
#define FILE_TYPE_NAME_UNKNOWN "unknown"

namespace cppfind {
    enum class FileType {UNKNOWN, ARCHIVE, AUDIO, BINARY, CODE, FONT, IMAGE, TEXT, VIDEO, XML};

    class FileTypes {
    public:
        FileTypes();
        ~FileTypes() = default;
        static FileType from_name(std::string_view name);
        static std::string to_name(const FileType& file_type);
        [[nodiscard]] FileType get_file_type_for_file_name(std::string_view file_name);
        [[nodiscard]] FileType get_file_type_for_extension(std::string_view file_ext);
        [[nodiscard]] FileType get_file_type_for_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_archive_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_audio_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_binary_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_code_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_font_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_image_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_text_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_unknown_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_video_path(const std::filesystem::path& file_path);
        [[nodiscard]] bool is_xml_path(const std::filesystem::path& file_path);

    private:
        sqlite3 *db;
        std::unordered_map<std::string, FileType> ext_type_cache;
        std::unordered_map<std::string, FileType> name_type_cache;
        bool name_type_cache_loaded = false;
        [[nodiscard]] FileType get_file_type_for_query_and_params(std::string_view query,
                                                                  const std::vector<std::string_view>& params) const;
        [[nodiscard]] std::unordered_map<std::string, FileType> get_file_types_for_query_and_params(std::string_view query,
                                                                                                    const std::vector<std::string_view>& params) const;
        void load_name_type_cache();
    };
}

#endif // CPPFIND_FILETYPES_H
