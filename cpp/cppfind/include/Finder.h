#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include <filesystem>
#include "FileResult.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    public:
        explicit Finder(const FindSettings& settings);
        std::optional<FileResult> filter_to_file_result(std::filesystem::path&& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_result(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_file_type(const FileType& file_type) const;
        [[nodiscard]] bool is_matching_file_result(const FileResult& file_result) const;
        std::vector<FileResult> find();

    private:
        FileTypes m_file_types;
        FindSettings m_settings;
        static void validate_settings(const FindSettings& settings);
        std::vector<FileResult> get_file_results(const std::filesystem::path& file_path, int depth);
        void sort_file_results(std::vector<FileResult>& file_results) const;
    };
}

#endif // CPPFIND_FINDER_H
