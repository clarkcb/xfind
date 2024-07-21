#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "config.h"

void get_home_path(char *dest)
{
    char *home_path = getenv("HOME");
    if (dest == NULL || home_path == NULL) {
        return;
    }
    dest[0] = '\0';
    strcat(dest, home_path);
    dest[strnlen(home_path, MAX_PATH_LENGTH)] = '\0';
}

void get_xfind_path(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    char *xfind_path = getenv("XFIND_PATH");
    if (xfind_path == NULL) {
        char home_path[MAX_PATH_LENGTH];
        get_home_path(home_path);
        strcat(dest, home_path);
        strcat(dest, "/src/xfind");
    } else {
        strcat(dest, xfind_path);
    }
}

void get_file_types_path(char *dest)
{
    char xfind_path[MAX_PATH_LENGTH + 11]; // + "/src/xfind\0"
    get_xfind_path(xfind_path);
    strcat(dest, xfind_path);
    strcat(dest, "/");
    strcat(dest, FILE_TYPES_REL_PATH);
}

void get_find_options_path(char *dest)
{
    char xfind_path[MAX_PATH_LENGTH + 11]; // + "/src/xfind\0"
    get_xfind_path(xfind_path);
    strcat(dest, xfind_path);
    strcat(dest, "/");
    strcat(dest, FIND_OPTIONS_REL_PATH);
}
