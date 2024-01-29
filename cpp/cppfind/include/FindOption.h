#ifndef CPPFIND_FINDOPTION_H
#define CPPFIND_FINDOPTION_H

#include <string>

namespace cppfind {
    class FindOption {
    public:
        FindOption(std::string_view short_arg, std::string_view long_arg, std::string_view description);
        [[nodiscard]] std::string short_arg() const;
        [[nodiscard]] std::string long_arg() const;
        [[nodiscard]] std::string description() const;
        [[nodiscard]] std::string sort_arg() const;

    private:
        std::string m_short_arg;
        std::string m_long_arg;
        std::string m_description;
        std::string m_sort_arg;
    };
}

#endif // CPPFIND_FINDOPTION_H
