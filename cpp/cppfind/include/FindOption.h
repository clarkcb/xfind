#ifndef CPPFIND_FINDOPTION_H
#define CPPFIND_FINDOPTION_H

#include <string>

#include "Option.h"

namespace cppfind {
    class FindOption : public Option {
    public:
        FindOption(std::string_view short_arg, std::string_view long_arg, std::string_view description, int arg_type);
        FindOption() = delete;
        [[nodiscard]] std::string short_arg() const override;
        [[nodiscard]] std::string long_arg() const override;
        [[nodiscard]] std::string description() const override;
        [[nodiscard]] int arg_type() const override;
        [[nodiscard]] std::string sort_arg() const override;

    private:
        std::string m_short_arg;
        std::string m_long_arg;
        std::string m_description;
        int m_arg_type;
        std::string m_sort_arg;
    };
}

#endif // CPPFIND_FINDOPTION_H
