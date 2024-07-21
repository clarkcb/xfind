#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "stringarray.h"

StringArray *new_string_array(void)
{
    StringArray *string_array = malloc(sizeof(StringArray));
    assert(string_array != NULL);
    string_array->strings = NULL;
    string_array->size = 0;
    return string_array;
}

StringArray *new_string_array_with_size(size_t size)
{
    StringArray *string_array = malloc(sizeof(StringArray));
    assert(string_array != NULL);
    string_array->strings = malloc(size * sizeof(char*));
    string_array->size = 0;
    return string_array;
}

void add_string_to_string_array(const char *s, StringArray *arr)
{
    // NOTE: this assumes the array is already defined as large enough to add a new value
    size_t slen = strnlen(s, 1024);
    arr->strings[arr->size] = malloc((slen + 1) * sizeof(char));
    strncpy(arr->strings[arr->size], s, slen);
    arr->strings[arr->size][slen] = '\0';
    arr->size++;
}

// includes 2 chars for enclosing '[' and ']',
// 2 chars per string for double quotes
// and arr_size - 1 for commas
size_t string_array_strlen(const StringArray *arr)
{
    size_t arrlen = 2; // for  '[' and ']'
    for (int i=0; i < arr->size; i++) {
        arrlen += strnlen(arr->strings[i], 1024) + 2; // for ""
    }
    if (arr->size > 0) {
        arrlen += (arr->size - 1); // for commas
    }
    return arrlen;
}

int index_of_string_in_string_array(const char *s, const StringArray *arr)
{
    int j = -1;
    for (int i = 0; i < arr->size; i++) {
        // char *as = arr->strings[i];
        if (strncmp(s, arr->strings[i], 20) == 0) {
            j = i;
            break;
        }
     }
    return j;
}

// TODO: implement (if needed)
char *string_array_to_string(char *s, StringArray *arr)
{
    return "[]";
}

void destroy_string_array(StringArray *arr)
{
    for (int i = 0; i < arr->size; i++) {
        free(arr->strings[i]);
    }
    free(arr->strings);
    free(arr);
}
