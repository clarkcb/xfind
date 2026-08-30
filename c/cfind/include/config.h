#ifndef CFIND_CONFIG_H
#define CFIND_CONFIG_H

#define MAX_HOMEPATH_LENGTH 100
#define DEFAULT_XFIND_CONFIG_REL_DIR ".config/xfind"
#define DEFAULT_XFIND_REL_PATH "src/xfind"
#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"

void get_home_path(char *dest);

void get_xfind_path(char *dest);

void get_file_types_path(char *dest);

void get_find_options_path(char *dest);

void get_default_find_settings_path(char *dest);

#endif
