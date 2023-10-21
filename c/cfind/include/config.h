#ifndef CONFIG_H
#define CONFIG_H

#define MAX_HOMEPATH_LENGTH 100
#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"

void get_home_path(char *dest);

void get_xfind_path(char *dest);

void get_file_types_path(char *dest);

void get_find_options_path(char *dest);

#endif
