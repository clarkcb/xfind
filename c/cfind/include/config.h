#ifndef CFIND_CONFIG_H
#define CFIND_CONFIG_H

#define MAX_HOMEPATH_LENGTH 100
#define FILE_TYPES_REL_PATH "shared/filetypes.json"
#define FIND_OPTIONS_REL_PATH "shared/findoptions.json"
#define XFIND_DB_REL_PATH "shared/xfind.db"

void get_home_path(char *dest);

void get_xfind_path(char *dest);

void get_file_types_path(char *dest);

void get_find_options_path(char *dest);

void get_xfind_db_path(char *dest);

#endif
