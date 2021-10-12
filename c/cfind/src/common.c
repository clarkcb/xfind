#include <stdio.h>
#include <string.h>

#include "common.h"

void log_msg(const char *msg)
{
    printf("%s\n", msg);
}

void log_err(const char *msg)
{
    printf("\nERROR: %s\n", msg);
}

int char_in_string(const char c, const char *s)
{
    char* cptr = strchr(s, c);
    return (cptr == NULL) ? 0 : 1;
}

int index_of_char_in_string(const char c, const char *s)
{
    char *p = strchr(s, c);
    if (p)
        return (int)(p - s);
    return -1;
}

int last_index_of_char_in_string(const char c, const char *s)
{
    char *p = strrchr(s, c);
    if (p)
        return (int)(p - s);
    return -1;
}

int index_of_string_in_array(const char *s, char **arr, size_t arr_size)
{
    int j = -1;
    for (int i = 0; i < arr_size; i++) {
      if (strncmp(s, arr[i], 20) == 0) {
            j = i;
            break;
        }
    }
    return j;
}

size_t get_longest_strlen(const char **arr, size_t arr_size)
{
    size_t longest_len = 0;
    for (int i=0; i < arr_size; i++) {
        size_t len = strlen(arr[i]);
        if (len > longest_len) {
            longest_len = len;
        }
    }
    return longest_len;
}
