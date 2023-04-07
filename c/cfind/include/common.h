#ifndef COMMON_H
#define COMMON_H

#include <stdlib.h>

void log_msg(const char *msg);

void log_err(const char *msg);

int char_in_string(const char c, const char *s);

int char_count_in_string(const char c, const char *s);

int index_of_char_in_string(const char c, const char *s);

int last_index_of_char_in_string(const char c, const char *s);

int index_of_string_in_array(const char *s, char **arr, size_t arr_size);

size_t get_longest_strlen(const char **arr, size_t arr_size);

unsigned int num_digits_ulong(unsigned long num);

void time_to_datestring(long t, char *s);

#endif
