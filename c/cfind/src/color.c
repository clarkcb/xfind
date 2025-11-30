#include <ctype.h>
#include <string.h>

#include "color.h"

#include "consolecolor.h"

Color color_from_name(const char *name)
{
    const size_t maxlen = 10;
    size_t namelen = strnlen(name, maxlen);
    size_t minlen = namelen < maxlen ? namelen : maxlen;
    char lname[10] = {0};
    strncpy(lname, name, minlen);
    for (int i = 0; i < minlen; i++) {
        char c = (char)tolower(name[i]);
        lname[i] = c;
    }
    if (strncmp(lname, COLOR_NAME_BLACK, maxlen) == 0) {
        return BLACK;
    }
    if (strncmp(lname, COLOR_NAME_RED, maxlen) == 0) {
        return RED;
    }
    if (strncmp(lname, COLOR_NAME_GREEN, maxlen) == 0) {
        return GREEN;
    }
    if (strncmp(lname, COLOR_NAME_YELLOW, maxlen) == 0) {
        return YELLOW;
    }
    if (strncmp(lname, COLOR_NAME_BLUE, maxlen) == 0) {
        return BLUE;
    }
    if (strncmp(lname, COLOR_NAME_MAGENTA, maxlen) == 0) {
        return MAGENTA;
    }
    if (strncmp(lname, COLOR_NAME_CYAN, maxlen) == 0) {
        return CYAN;
    }
    if (strncmp(lname, COLOR_NAME_WHITE, maxlen) == 0) {
        return WHITE;
    }
    return BLACK;
}

void color_to_name(const Color color, char *name)
{
    switch(color) {
        case BLACK:
            strncpy(name, COLOR_NAME_BLACK, 5);
            name[5] = '\0';
            break;
        case RED:
            strncpy(name, COLOR_NAME_RED, 3);
            name[3] = '\0';
            break;
        case GREEN:
            strncpy(name, COLOR_NAME_GREEN, 5);
            name[5] = '\0';
            break;
        case YELLOW:
            strncpy(name, COLOR_NAME_YELLOW, 6);
            name[6] = '\0';
            break;
        case BLUE:
            strncpy(name, COLOR_NAME_BLUE, 4);
            name[4] = '\0';
            break;
        case MAGENTA:
            strncpy(name, COLOR_NAME_MAGENTA, 7);
            name[7] = '\0';
            break;
        case CYAN:
            strncpy(name, COLOR_NAME_CYAN, 4);
            name[4] = '\0';
            break;
        case WHITE:
            strncpy(name, COLOR_NAME_WHITE, 5);
            name[5] = '\0';
            break;
        default:
            strncpy(name, COLOR_NAME_BLACK, 5);
            name[5] = '\0';
    }
}

void color_to_console_color(const Color color, char *console_color)
{
    switch(color) {
        case BLACK:
            strncpy(console_color, CONSOLE_COLOR_BLACK, 7);
            console_color[7] = '\0';
            break;
        case RED:
            strncpy(console_color, CONSOLE_COLOR_RED, 7);
            console_color[7] = '\0';
            break;
        case GREEN:
            strncpy(console_color, CONSOLE_COLOR_GREEN, 7);
            console_color[7] = '\0';
            break;
        case YELLOW:
            strncpy(console_color, CONSOLE_COLOR_YELLOW, 7);
            console_color[7] = '\0';
            break;
        case BLUE:
            strncpy(console_color, CONSOLE_COLOR_BLUE, 7);
            console_color[7] = '\0';
            break;
        case MAGENTA:
            strncpy(console_color, CONSOLE_COLOR_MAGENTA, 7);
            console_color[7] = '\0';
            break;
        case CYAN:
            strncpy(console_color, CONSOLE_COLOR_CYAN, 7);
            console_color[7] = '\0';
            break;
        case WHITE:
            strncpy(console_color, CONSOLE_COLOR_WHITE, 7);
            console_color[7] = '\0';
            break;
        default:
            strncpy(console_color, CONSOLE_COLOR_BLACK, 7);
            console_color[7] = '\0';
    }
}
