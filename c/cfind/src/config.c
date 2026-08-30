#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "config.h"

void get_home_path(char *dest)
{
    const char *home_path = getenv("HOME");
    if (dest == NULL || home_path == NULL) {
        return;
    }
    dest[0] = '\0';
    strcat(dest, home_path);
    dest[strnlen(home_path, MAX_PATH_LENGTH)] = '\0';
}

void get_xfind_config_dir(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    const char *xfind_config_dir = getenv("XFIND_CONFIG_DIR");
    if (xfind_config_dir == NULL) {
        char home_path[MAX_PATH_LENGTH];
        get_home_path(home_path);
        const size_t home_len = strnlen(home_path, MAX_PATH_LENGTH);
        strcat(dest, home_path);
        strcat(dest, PATH_SEPARATOR_S);
        const size_t xfind_config_rel_dir_len = strnlen(DEFAULT_XFIND_CONFIG_REL_DIR, MAX_PATH_LENGTH);
        strcat(dest, DEFAULT_XFIND_CONFIG_REL_DIR);
        dest[home_len + xfind_config_rel_dir_len + 2] = '\0';
    } else {
        const size_t xfind_config_dir_len = strnlen(xfind_config_dir, MAX_PATH_LENGTH);
        strcat(dest, xfind_config_dir);
        dest[xfind_config_dir_len] = '\0';
    }
}

void get_xfind_path(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    const char *xfind_path = getenv("XFIND_PATH");
    if (xfind_path == NULL) {
        char home_path[MAX_PATH_LENGTH];
        get_home_path(home_path);
        const size_t home_len = strnlen(home_path, MAX_PATH_LENGTH);
        strcat(dest, home_path);
        strcat(dest, PATH_SEPARATOR_S);
        const size_t xfind_rel_path_len = strnlen(DEFAULT_XFIND_REL_PATH, MAX_PATH_LENGTH);
        strcat(dest, DEFAULT_XFIND_REL_PATH);
        dest[home_len + xfind_rel_path_len + 2] = '\0';
    } else {
        const size_t xfind_len = strnlen(xfind_path, MAX_PATH_LENGTH);
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
    char xfind_path[MAX_PATH_LENGTH];
    get_xfind_path(xfind_path);
    const size_t xfind_len = strnlen(xfind_path, MAX_PATH_LENGTH);
    strcat(dest, xfind_path);
    strcat(dest, PATH_SEPARATOR_S);
    const size_t file_types_rel_path_len = strnlen(FILE_TYPES_REL_PATH, MAX_PATH_LENGTH);
    strcat(dest, FILE_TYPES_REL_PATH);
    dest[xfind_len + file_types_rel_path_len + 1] = '\0';
}

void get_find_options_path(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    char xfind_path[MAX_PATH_LENGTH];
    get_xfind_path(xfind_path);
    const size_t xfind_len = strnlen(xfind_path, MAX_PATH_LENGTH);
    strcat(dest, xfind_path);
    strcat(dest, PATH_SEPARATOR_S);
    const size_t find_options_rel_path_len = strnlen(FIND_OPTIONS_REL_PATH, MAX_PATH_LENGTH);
    strcat(dest, FIND_OPTIONS_REL_PATH);
    dest[xfind_len + find_options_rel_path_len + 1] = '\0';
}

void get_default_find_settings_path(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    char xfind_config_dir[MAX_PATH_LENGTH];
    get_xfind_config_dir(xfind_config_dir);
    const size_t xfind_config_dir_len = strnlen(xfind_config_dir, MAX_PATH_LENGTH);
    strcat(dest, xfind_config_dir);
    strcat(dest, PATH_SEPARATOR_S);
    strcat(dest, "settings.json");
    dest[xfind_config_dir_len + 15] = '\0';
}
