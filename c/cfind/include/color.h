#ifndef COLOR_H
#define COLOR_H

#define COLOR_NAME_BLACK "black"
#define COLOR_NAME_RED "red"
#define COLOR_NAME_GREEN "green"
#define COLOR_NAME_YELLOW "yellow"
#define COLOR_NAME_BLUE "blue"
#define COLOR_NAME_MAGENTA "magenta"
#define COLOR_NAME_CYAN "cyan"
#define COLOR_NAME_WHITE "white"

typedef enum {
    BLACK = 1,
    RED = 2,
    GREEN = 3,
    YELLOW = 4,
    BLUE = 5,
    MAGENTA = 6,
    CYAN = 7,
    WHITE = 8
} Color;

Color color_from_name(const char *name);

void color_to_name(Color color, char *name);

void color_to_console_color(Color color, char *console_color);

#endif
