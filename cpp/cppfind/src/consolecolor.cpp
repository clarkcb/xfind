#include <iostream>

#include "common.h"

#include "consolecolor.h"

namespace cppfind {

    std::string color_to_console_color(const Color color) {
        switch (color)
        {
            case Color::BLACK:
                return CONSOLE_BLACK;
            case Color::RED:
                return CONSOLE_RED;
            case Color::GREEN:
                return CONSOLE_GREEN;
            case Color::YELLOW:
                return CONSOLE_YELLOW;
            case Color::BLUE:
                return CONSOLE_BLUE;
            case Color::MAGENTA:
                return CONSOLE_MAGENTA;
            case Color::CYAN:
                return CONSOLE_CYAN;
            case Color::WHITE:
                return CONSOLE_WHITE;
        }
        return CONSOLE_BLACK;
    }
}
