#ifndef CPPFIND_FILETYPES_H
#define CPPFIND_FILETYPES_H

#include <filesystem>
#include <sqlite3.h>

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
        [[nodiscard]] FileType get_file_type_for_file_name(std::string_view file_name) const;
        [[nodiscard]] FileType get_file_type_for_extension(std::string_view file_ext) const;
        [[nodiscard]] FileType get_file_type_for_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_archive_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_audio_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_binary_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_code_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_font_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_image_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_text_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_unknown_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_video_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_xml_path(const std::filesystem::path& file_path) const;

    private:
        sqlite3 *db;
        [[nodiscard]] FileType get_file_type_for_query_and_elem(const char* query, const char* elem) const;
    };
}

#endif // CPPFIND_FILETYPES_H
