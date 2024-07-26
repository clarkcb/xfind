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
        size_t home_len = strnlen(home_path, MAX_PATH_LENGTH);
        strcat(dest, home_path);
        strcat(dest, PATH_SEPARATOR_S);
        strcat(dest, "src");
        strcat(dest, PATH_SEPARATOR_S);
        strcat(dest, "xfind");
        dest[home_len + 10] = '\0';
    } else {
        size_t xfind_len = strnlen(xfind_path, MAX_PATH_LENGTH);
        strcat(dest, xfind_path);
        dest[xfind_len] = '\0';
    }
}

void get_file_types_path(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    char xfind_path[MAX_PATH_LENGTH + 11]; // + "/src/xfind\0"
    get_xfind_path(xfind_path);
    size_t xfind_len = strnlen(xfind_path, MAX_PATH_LENGTH);
    strcat(dest, xfind_path);
    strcat(dest, PATH_SEPARATOR_S);
    size_t file_types_rel_path_len = strnlen(FILE_TYPES_REL_PATH, MAX_PATH_LENGTH);
    strcat(dest, FILE_TYPES_REL_PATH);
    dest[xfind_len + file_types_rel_path_len + 1] = '\0';
}

void get_find_options_path(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    char xfind_path[MAX_PATH_LENGTH + 11]; // + "/src/xfind\0"
    get_xfind_path(xfind_path);
    size_t xfind_len = strnlen(xfind_path, MAX_PATH_LENGTH);
    strcat(dest, xfind_path);
    strcat(dest, PATH_SEPARATOR_S);
    size_t find_options_rel_path_len = strnlen(FIND_OPTIONS_REL_PATH, MAX_PATH_LENGTH);
    strcat(dest, FIND_OPTIONS_REL_PATH);
    dest[xfind_len + find_options_rel_path_len + 1] = '\0';
}

void get_xfind_db_path(char *dest)
{
    char xfind_path[MAX_PATH_LENGTH + 11]; // + "/src/xfind\0"
    get_xfind_path(xfind_path);
    strcat(dest, xfind_path);
    strcat(dest, "/");
    strcat(dest, XFIND_DB_REL_PATH);
}
