#ifndef STRINGARRAY_H
#define STRINGARRAY_H

typedef struct StringArray {
    char **strings;
    size_t size;
} StringArray;

StringArray *new_string_array(void);

StringArray *new_string_array_with_size(size_t size);

void add_string_to_string_array(const char *s, StringArray *arr);

size_t string_array_strlen(const StringArray *arr);

int index_of_string_in_string_array(const char *s, const StringArray *arr);

char *string_array_to_string(char *s, StringArray *arr);

void destroy_string_array(StringArray *arr);

#endif
