#ifndef CFIND_COMMON_H
#define CFIND_COMMON_H

#include <stdlib.h>

#define MAX_FILENAME_LENGTH 255
#define MAX_PATH_LENGTH 1024
#define MAX_STRING_LENGTH 1024

void log_msg(const char *msg);

void log_err(const char *msg);

int char_in_string(char c, const char *s);

int char_count_in_string(char c, const char *s);

int index_of_char_in_string(char c, const char *s);

int last_index_of_char_in_string(char c, const char *s);

int index_of_string_in_array(const char *s, char **arr, size_t arr_size);

size_t get_longest_strlen(const char **arr, size_t arr_size);

unsigned int num_digits_ulong(unsigned long num);

void time_to_datestring(long t, char *s);

#endif
