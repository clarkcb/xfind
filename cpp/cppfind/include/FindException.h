#ifndef CPPFIND_FINDEXCEPTION_H
#define CPPFIND_FINDEXCEPTION_H

#include <string>

namespace cppfind {
    class FindException : public std::exception {
    public:
        explicit FindException(const std::string& message);
        const char *what() const throw();

    private:
        std::string m_message;
    };
}

#endif //CPPFIND_FINDEXCEPTION_H
