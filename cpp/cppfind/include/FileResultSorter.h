#ifndef CPPFIND_FILERESULTSORTER_H
#define CPPFIND_FILERESULTSORTER_H

#include "FileResult.h"
#include "FindSettings.h"

namespace cppfind {
    class FileResultSorter {
    public:
        explicit FileResultSorter(const FindSettings& settings);
        explicit FileResultSorter(const std::unique_ptr<FindSettings>& settings_ptr);
        FileResultSorter(FileResultSorter& other) = delete;
        FileResultSorter(FileResultSorter&& other) = delete;
        [[nodiscard]] FindSettings settings() const;
        void sort(std::vector<FileResult>& file_results) const;

    private:
        const FindSettings m_settings;
        [[nodiscard]] std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_path() const;
        [[nodiscard]] std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_name() const;
        [[nodiscard]] std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_size() const;
        [[nodiscard]] std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_type() const;
        [[nodiscard]] std::function<bool(FileResult&, FileResult&)> get_cmp_file_results_by_lastmod() const;
        [[nodiscard]] std::function<bool(FileResult&, FileResult&)> get_file_result_comparator() const;
    };

    int cmp_file_results_by_path(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_path_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_name(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_name_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_size(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_size_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_type(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_type_ci(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_lastmod(const FileResult& fr1, const FileResult& fr2);
    int cmp_file_results_by_lastmod_ci(const FileResult& fr1, const FileResult& fr2);
}


#endif // CPPFIND_FILERESULTSORTER_H
