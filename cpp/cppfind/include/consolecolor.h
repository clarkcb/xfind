#ifndef CPPFIND_CONSOLECOLOR_H
#define CPPFIND_CONSOLECOLOR_H

#define CONSOLE_RESET   "\033[0m"
#define CONSOLE_BLACK   "\033[0;30m"
#define CONSOLE_RED     "\033[0;31m"
#define CONSOLE_GREEN   "\033[0;32m"
#define CONSOLE_YELLOW  "\033[0;33m"
#define CONSOLE_BLUE    "\033[0;34m"
#define CONSOLE_MAGENTA "\033[0;35m"
#define CONSOLE_CYAN    "\033[0;36m"
#define CONSOLE_WHITE   "\033[0;37m"

#define BOLD_BLACK      "\033[1;30m"
#define BOLD_RED        "\033[1;31m"
#define BOLD_GREEN      "\033[1;32m"
#define BOLD_YELLOW     "\033[1;33m"
#define BOLD_BLUE       "\033[1;34m"
#define BOLD_MAGENTA    "\033[1;35m"
#define BOLD_CYAN       "\033[1;36m"
#define BOLD_WHITE      "\033[1;37m"

namespace cppfind {
    enum class Color {BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE};

    Color color_from_name(std::string_view name);
    std::string color_to_name(Color color);
    std::string color_to_console_color(Color color);
}

#endif //CPPFIND_CONSOLECOLOR_H
