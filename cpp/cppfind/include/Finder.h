#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include <filesystem>
#include "FileResult.h"
#include "FileResultFormatter.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    public:
        // Finder() noexcept;
        explicit Finder(const FindSettings& settings);
        explicit Finder(const std::unique_ptr<FindSettings>& settings_ptr);
        Finder(Finder& other) = delete;
        Finder(Finder&& other) = delete;
        [[nodiscard]] std::optional<FileResult> filter_to_file_result(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_result(const FileResult& file_result) const;
        [[nodiscard]] bool filter_dir_path_by_hidden(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool filter_dir_path_by_in_patterns(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool filter_dir_path_by_out_patterns(const std::filesystem::path& dir_path) const;
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
        [[nodiscard]] std::vector<FileResult> find() const;

    private:
        FileTypes m_file_types;
        FindSettings m_settings;
        static void validate_settings(const FindSettings& settings);
        [[nodiscard]] std::vector<FileResult> get_file_results(const std::filesystem::path& file_path) const;
        [[nodiscard]] std::vector<FileResult> rec_get_file_results(const std::filesystem::path& dir_path, int min_depth,
            int max_depth, int current_depth) const;
    };

    std::vector<std::filesystem::path> get_matching_dir_paths(const std::vector<FileResult>& file_results);
    void print_file_result_dirs(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter);
    void print_file_results(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter);
}

#endif // CPPFIND_FINDER_H
