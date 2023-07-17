#include "RegexPattern.h"

namespace cppfind {
    RegexPattern::RegexPattern(const std::string& pattern) {
        m_pattern = pattern;
        m_regex = std::regex(pattern);
    }

    std::string RegexPattern::pattern() const {
        return m_pattern;
    }

    std::regex RegexPattern::r() const {
        return m_regex;
    }
}
