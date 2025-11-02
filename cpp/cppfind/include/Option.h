#ifndef CPPFIND_OPTION_H
#define CPPFIND_OPTION_H

#include <string>

namespace cppfind {
    class Option {
    public:
        [[nodiscard]] virtual std::string short_arg() const = 0;
        [[nodiscard]] virtual std::string long_arg() const = 0;
        [[nodiscard]] virtual std::string description() const = 0;
        [[nodiscard]] virtual int arg_type() const = 0;
        [[nodiscard]] virtual std::string sort_arg() const = 0;
        virtual ~Option() = default;
    };
}

#endif // CPPFIND_OPTION_H
