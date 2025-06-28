#ifndef CPPFIND_FILERESULTFORMATTER_H
#define CPPFIND_FILERESULTFORMATTER_H

#include <filesystem>
#include <string>
#include "FileResult.h"
#include "FindSettings.h"

namespace cppfind {
    class FileResultFormatter {
    public:
        explicit FileResultFormatter(const FindSettings& settings);
        explicit FileResultFormatter(const std::unique_ptr<FindSettings>& settings_ptr);
        FileResultFormatter(FileResultFormatter& other) = delete;
        FileResultFormatter(FileResultFormatter&& other) = delete;
        [[nodiscard]] FindSettings settings() const;
        static std::string colorize(const std::string& s, unsigned long match_start_idx, unsigned long match_end_idx);
        [[nodiscard]] std::string format_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] std::string format_file_name(const std::string& file_name) const;
        [[nodiscard]] std::string format_file_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] std::string format_file_result(const FileResult& result) const;

    private:
        void init();
        const FindSettings m_settings;
        std::function<std::string(const std::filesystem::path&)> func_format_dir_path;
        std::function<std::string(const std::string&)> func_format_file_name;
        [[nodiscard]] std::string format_dir_path_with_color(const std::filesystem::path& dir_path) const;
        [[nodiscard]] std::string format_file_name_with_color(const std::string& file_name) const;
    };
}

#endif // CPPFIND_FILERESULTFORMATTER_H
