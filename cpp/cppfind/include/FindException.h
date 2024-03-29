#ifndef CPPFIND_FINDEXCEPTION_H
#define CPPFIND_FINDEXCEPTION_H

#include <string>

namespace cppfind {
    class FindException : public std::exception {
    public:
        explicit FindException(std::string_view message);
        [[nodiscard]] std::string message() const noexcept;
        [[nodiscard]] const char *what() const noexcept override;

    private:
        std::string m_message;
    };
}

#endif // CPPFIND_FINDEXCEPTION_H
