#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"

void log_msg(const char *msg)
{
    printf("%s\n", msg);
    fflush(stdout);
}

void log_err(const char *msg)
{
    fprintf(stderr, "\nERROR: %s\n", msg);
}

bool is_null_or_empty_string(const char *s)
{
    if (s == NULL || strncmp(s, "", 1) == 0) {
        return 1;
    }
    return 0;
}

int char_in_string(const char c, const char *s)
{
    const char* cptr = strchr(s, c);
    return (cptr == NULL) ? 0 : 1;
}

int char_count_in_string(const char c, const char *s)
{
    int char_count = 0;
    for (int i = 0; i < strnlen(s, MAX_STRING_LENGTH); i++) {
        if (s[i] == c) {
            char_count++;
        }
    }
    return char_count;
}

int index_of_char_in_string(const char c, const char *s)
{
    char *p = strchr(s, c);
    if (p)
        return p - s;
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
        size_t len = strnlen(arr[i], MAX_STRING_LENGTH);
        if (len > longest_len) {
            longest_len = len;
        }
    }
    return longest_len;
}

unsigned int num_digits_ulong(unsigned long num)
{
    if (num <         10) return 1;
    if (num <        100) return 2;
    if (num <       1000) return 3;
    if (num <      10000) return 4;
    if (num <     100000) return 5;
    if (num <    1000000) return 6;      
    if (num <   10000000) return 7;
    if (num <  100000000) return 8;
    if (num < 1000000000) return 9;
    return 10;
}

void time_to_datestring(const long t, char *datestr)
{
    if (t == 0L) {
        sprintf(datestr, "0");
        return;
    }

    char buf[11];
    struct tm *tm = localtime(&t);
    
    if (strftime(buf, sizeof(buf), "%Y-%m-%d", tm) == 0) {
        fprintf(stderr, "ERROR: strftime returned 0\n");
        sprintf(datestr, "0");
    } else {
        sprintf(datestr, "%s", buf);
    }

    // datestr[10] = '\0';
}
