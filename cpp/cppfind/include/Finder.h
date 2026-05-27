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
        [[nodiscard]] std::optional<FileResult> filter_archive_file_path_to_file_result(const std::filesystem::path& file_path, FileType file_type) const;
        [[nodiscard]] std::optional<FileResult> filter_regular_file_path_to_file_result(const std::filesystem::path& file_path, FileType file_type) const;
        [[nodiscard]] std::optional<FileResult> filter_to_file_result(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_traversable_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_empty_or_matching_dir_path(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_archive_extension(const std::string& file_ext) const;
        [[nodiscard]] bool has_matching_archive_extension(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_name(const std::string& file_name) const;
        [[nodiscard]] bool has_matching_archive_file_name(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_archive_file_result(const FileResult& file_result) const;
        [[nodiscard]] bool is_matching_extension(const std::string& file_ext) const;
        [[nodiscard]] bool has_matching_extension(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_file_name(const std::string& file_name) const;
        [[nodiscard]] bool has_matching_file_name(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_file_path(const std::filesystem::path& file_path) const;
        [[nodiscard]] bool is_matching_file_type(const FileType& file_type) const;
        [[nodiscard]] bool is_matching_file_size(uint64_t file_size) const;
        [[nodiscard]] bool is_matching_last_mod(long last_mod) const;
        [[nodiscard]] bool is_matching_file_result(const FileResult& file_result) const;
        [[nodiscard]] std::vector<FileResult> find() const;

    private:
        FileTypes m_file_types;
        FindSettings m_settings;
        static void validate_settings(const FindSettings& settings, const FileTypes& file_types);

        [[nodiscard]] bool is_matching_dir_path_by_hidden(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_dir_path_by_in_patterns(const std::filesystem::path& dir_path) const;
        [[nodiscard]] bool is_matching_dir_path_by_out_patterns(const std::filesystem::path& dir_path) const;

        // [[nodiscard]] bool is_matching_file_name_by_hidden(const std::string& file_name) const;

        [[nodiscard]] std::pair<uint64_t, long> get_file_path_size_and_last_mod(const std::filesystem::path& file_path) const;

        [[nodiscard]] std::vector<FileResult> get_file_results(const std::filesystem::path& path) const;
        [[nodiscard]] std::vector<FileResult> rec_get_file_results(const std::filesystem::path& dir_path, int min_depth,
            int max_depth, int current_depth) const;
    };


    static bool matches_any_pattern(std::string_view s, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool any_matches_any_pattern(const std::vector<std::string>& ss, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool empty_or_matches_any_pattern(std::string_view s, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool empty_or_not_matches_any_pattern(std::string_view s, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool empty_or_matches_any_string(std::string_view s, const std::unordered_set<std::string>& string_set);
    static bool empty_or_not_matches_any_string(std::string_view s, const std::unordered_set<std::string>& string_set);
    static bool empty_or_matches_any_file_type(const FileType& file_type, const std::unordered_set<FileType>& file_types);
    static bool empty_or_not_matches_any_file_type(const FileType& file_type, const std::unordered_set<FileType>& file_types);
    static bool path_matches_any_pattern(const std::filesystem::path& path, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool path_not_matches_any_pattern(const std::filesystem::path& path, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool empty_or_path_matches_any_pattern(const std::filesystem::path& path, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool empty_or_path_not_matches_any_pattern(const std::filesystem::path& path, const std::unordered_set<RegexPattern, RegexPatternHash>& patterns);
    static bool is_matching_path_by_symlink(const std::filesystem::path& path, bool follow_symlinks);
    static bool is_matching_path_by_hidden(const std::filesystem::path& path, bool include_hidden);
    static bool is_matching_file_name_by_hidden(const std::string& file_name, bool include_hidden);

    std::vector<std::filesystem::path> get_matching_dir_paths(const std::vector<FileResult>& file_results);
    void print_file_result_dirs(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter);
    void print_file_results(const std::vector<FileResult>& file_results, const FileResultFormatter& formatter);
}

#endif // CPPFIND_FINDER_H
