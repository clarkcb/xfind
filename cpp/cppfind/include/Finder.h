#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include <filesystem>
#include "FileResult.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    public:
        // Finder() noexcept;
        explicit Finder(const FindSettings& settings);
        explicit Finder(const std::unique_ptr<FindSettings>& settings_ptr);
        Finder(Finder& other) = delete;
        Finder(Finder&& other) = delete;
        std::optional<FileResult> filter_to_file_result(std::filesystem::path&& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_result(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_archive_extension(const std::string& file_ext) const;
        [[nodiscard]] bool has_matching_archive_extension(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_extension(const std::string& file_ext) const;
        [[nodiscard]] bool has_matching_extension(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_archive_file_name(const std::string& file_name) const;
        [[nodiscard]] bool is_matching_file_name(const std::string& file_name) const;
        [[nodiscard]] bool is_matching_file_type(const FileType& file_type) const;
        [[nodiscard]] bool is_matching_file_size(uint64_t file_size) const;
        [[nodiscard]] bool is_matching_last_mod(long last_mod) const;
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
