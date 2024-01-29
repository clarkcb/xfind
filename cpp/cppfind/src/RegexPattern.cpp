// add skeleton code for RegexPattern class

#include "RegexPattern.h"

namespace cppfind {
    RegexPattern::RegexPattern(const std::string_view pattern)
        : m_pattern(pattern), m_ignore_case(false), m_multi_line(false), m_dot_all(false), m_regex(std::regex(std::string{pattern})) {
    }

    RegexPattern::RegexPattern(const std::string_view pattern, const bool ignore_case, const bool multi_line,
                               const bool dot_all)
        : m_pattern(pattern), m_ignore_case(ignore_case), m_multi_line(multi_line), m_dot_all(dot_all) {
        std::regex::flag_type flags = std::regex::ECMAScript;
        if (ignore_case) {
            flags |= std::regex::icase;
        }
        if (multi_line) {
            flags |= std::regex::multiline;
        }
//        if (dot_all) {
//            flags |= std::regex::dotall;
//        }
        m_regex = std::regex(std::string{pattern}, flags);
    }

    std::string RegexPattern::pattern() const {
        return m_pattern;
    }

    bool RegexPattern::ignore_case() const {
        return m_ignore_case;
    }

    bool RegexPattern::multi_line() const {
        return m_multi_line;
    }

    bool RegexPattern::dot_all() const {
        return m_dot_all;
    }

    std::regex RegexPattern::regex() const {
        return m_regex;
    }

    std::string RegexPattern::string() const {
        return pattern();
    }
}
