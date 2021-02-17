#include "FindException.h"

namespace cppfind {
    FindException::FindException(const std::string& message) {
        m_message = message;
    }

    const char *FindException::what() const throw() {
        return m_message.c_str();
    }
}
