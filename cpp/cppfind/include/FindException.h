#ifndef CPPFIND_FINDEXCEPTION_H
#define CPPFIND_FINDEXCEPTION_H

#include <string>

#define INVALID_RANGE_MINDEPTH_MAXDEPTH "Invalid range for mindepth and maxdepth"
#define INVALID_RANGE_MINSIZE_MAXSIZE "Invalid range for minsize and maxsize"
#define INVALID_RANGE_MINLASTMOD_MAXLASTMOD "Invalid range for minlastmod and maxlastmod"
#define STARTPATH_NOT_DEFINED "Startpath not defined"
#define STARTPATH_NOT_FOUND "Startpath not found"
#define STARTPATH_NOT_READABLE "Startpath not readable"

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
