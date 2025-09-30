#include "ArgToken.h"

namespace cppfind {
    // implement the ArgToken constructor
    ArgToken::ArgToken(const std::string_view name, const uint8_t token_type, const std::any &value) :
    m_name{name}, m_token_type{token_type}, m_value{value} {
    }

    std::string ArgToken::name() const {
        return m_name;
    }

    uint8_t ArgToken::token_type() const {
        return m_token_type;
    }

    std::any ArgToken::value() const {
        return m_value;
    }
}
