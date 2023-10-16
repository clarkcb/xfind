#include "FindException.h"

namespace cppfind {
    FindException::FindException(const std::string& message) {
        m_message = message;
    }

    std::string FindException::message() const noexcept {
        return m_message;
    }

    const char *FindException::what() const noexcept {
        return m_message.c_str();
    }
}
