#ifndef CPPFIND_ARGTOKEN_H
#define CPPFIND_ARGTOKEN_H

#include <any>
#include <string>

namespace cppfind {
    class ArgToken {
    public:
        ArgToken(std::string_view name, uint8_t token_type, const std::any &value);
        ArgToken() = delete;
        [[nodiscard]] std::string name() const;
        [[nodiscard]] uint8_t token_type() const;
        [[nodiscard]] std::any value() const;

    private:
        std::string m_name;
        uint8_t m_token_type;
        std::any m_value;
    };
}

#endif // CPPFIND_ARGTOKEN_H
