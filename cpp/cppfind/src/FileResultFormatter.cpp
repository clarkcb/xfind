#include <regex>
#include "color.h"
#include "FileResultFormatter.h"

namespace cppfind {
    FileResultFormatter::FileResultFormatter(const FindSettings& settings) :  m_settings{settings} {
        init();
    }

    FileResultFormatter::FileResultFormatter(const std::unique_ptr<FindSettings>& settings_ptr) : m_settings{*settings_ptr} {
        init();
    }

    void FileResultFormatter::init() {
        if (m_settings.colorize() && !m_settings.in_dir_patterns().empty()) {
            func_format_dir_path = [this](const std::filesystem::path& dir_path) { return format_dir_path_with_color(dir_path); };
        } else {
            func_format_dir_path = [](const std::filesystem::path& dir_path) { return dir_path.string(); };
        }
        if (m_settings.colorize() && (!m_settings.in_extensions().empty() || !m_settings.in_file_patterns().empty())) {
            func_format_file_name = [this](const std::string& file_name) { return format_file_name_with_color(file_name); };
        } else {
            func_format_file_name = [](const std::string& file_name) { return file_name; };
        }
    }

    FindSettings FileResultFormatter::settings() const {
        return m_settings;
    }

    std::string FileResultFormatter::colorize(const std::string& s, const unsigned long match_start_idx,
        const unsigned long match_end_idx) {
        std::string colorized;
        colorized.reserve(s.length() + 8);
        if (match_start_idx > 0) {
            colorized.append(s.substr(0, match_start_idx));
        }
        colorized.append(COLOR_GREEN);
        colorized.append(s.substr(match_start_idx, match_end_idx - match_start_idx));
        colorized.append(COLOR_RESET);
        // if (match_end_idx < s.length() - 1) {
        if (match_end_idx < s.length()) {
            colorized.append(s.substr(match_end_idx));
        }
        return colorized;
    }

    std::string FileResultFormatter::format_dir_path_with_color(const std::filesystem::path& dir_path) const {
        std::string formatted_dir{"."};
        if (!dir_path.empty()) {
            formatted_dir = dir_path.string();
            std::smatch match;
            for (const auto& p : m_settings.in_dir_patterns()) {
                if (std::regex r = p.regex(); std::regex_search(formatted_dir, match, r)) {
                    const unsigned long match_start_idx = match.position(0);
                    const unsigned long match_end_idx = match_start_idx + match.length(0);
                    formatted_dir = colorize(formatted_dir, match_start_idx, match_end_idx);
                }
            }
        }
        return formatted_dir;
    }

    std::string FileResultFormatter::format_dir_path(const std::filesystem::path& dir_path) const {
        return func_format_dir_path(dir_path);
    }

    std::string FileResultFormatter::format_file_name_with_color(const std::string& file_name) const {
        std::string formatted_file_name{file_name};
        std::smatch match;
        for (const auto& p : m_settings.in_file_patterns()) {
            if (std::regex r = p.regex(); std::regex_search(formatted_file_name, match, r)) {
                const unsigned long match_start_idx = match.position(0);
                const unsigned long match_end_idx = match_start_idx + match.length(0);
                formatted_file_name = colorize(formatted_file_name, match_start_idx, match_end_idx);
            }
        }
        if (!m_settings.in_extensions().empty()) {
            if (const auto idx = formatted_file_name.find_last_of('.');
                idx > 0 && idx < formatted_file_name.length() - 1) {
                formatted_file_name = colorize(formatted_file_name, idx + 1, formatted_file_name.length());
            }
        }
        return formatted_file_name;
    }

    std::string FileResultFormatter::format_file_name(const std::string& file_name) const {
        return func_format_file_name(file_name);
    }

    std::string FileResultFormatter::format_file_path(const std::filesystem::path& file_path) const {
        const std::string parent = format_dir_path(file_path.parent_path());
        const std::string file_name = format_file_name(file_path.filename());
        return std::filesystem::path(parent) / file_name;
    }

    std::string FileResultFormatter::format_file_result(const FileResult& result) const {
        return format_file_path(result.file_path());
    }
}
