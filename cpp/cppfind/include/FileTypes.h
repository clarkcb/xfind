#ifndef CPPFIND_FILETYPES_H
#define CPPFIND_FILETYPES_H

#include <unordered_set>

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
        ~FileTypes();
        static FileType from_name(std::string_view name);
        static std::string to_name(const FileType& file_type);
        [[nodiscard]] FileType get_file_type(std::string_view file_path) const;
        [[nodiscard]] bool is_archive_file(std::string_view file_path) const;
        [[nodiscard]] bool is_audio_file(std::string_view file_path) const;
        [[nodiscard]] bool is_binary_file(std::string_view file_path) const;
        [[nodiscard]] bool is_code_file(std::string_view file_path) const;
        [[nodiscard]] bool is_font_file(std::string_view file_path) const;
        [[nodiscard]] bool is_image_file(std::string_view file_path) const;
        [[nodiscard]] bool is_text_file(std::string_view file_path) const;
        [[nodiscard]] bool is_unknown_file(std::string_view file_path) const;
        [[nodiscard]] bool is_video_file(std::string_view file_path) const;
        [[nodiscard]] bool is_xml_file(std::string_view file_path) const;

    private:
        std::unordered_set<std::string> m_archive_extensions;
        std::unordered_set<std::string> m_archive_names;
        std::unordered_set<std::string> m_audio_extensions;
        std::unordered_set<std::string> m_audio_names;
        std::unordered_set<std::string> m_binary_extensions;
        std::unordered_set<std::string> m_binary_names;
        std::unordered_set<std::string> m_code_extensions;
        std::unordered_set<std::string> m_code_names;
        std::unordered_set<std::string> m_font_extensions;
        std::unordered_set<std::string> m_font_names;
        std::unordered_set<std::string> m_image_extensions;
        std::unordered_set<std::string> m_image_names;
        std::unordered_set<std::string> m_text_extensions;
        std::unordered_set<std::string> m_text_names;
        std::unordered_set<std::string> m_video_extensions;
        std::unordered_set<std::string> m_video_names;
        std::unordered_set<std::string> m_xml_extensions;
        std::unordered_set<std::string> m_xml_names;
        void load_file_types();
    };
}

#endif // CPPFIND_FILETYPES_H
