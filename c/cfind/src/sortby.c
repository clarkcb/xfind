#include <ctype.h>
#include <string.h>

#include "sortby.h"

SortBy sort_by_from_name(const char *name)
{
    const size_t maxlen = 10;
    size_t namelen = strnlen(name, maxlen);
    size_t minlen = namelen < maxlen  ? namelen : maxlen;
    char lname[10] = {0};
    strncpy(lname, name, minlen);
    for (int i = 0; i < minlen; i++) {
        char c = (char)tolower(name[i]);
        lname[i] = c;
    }
    if (strncmp(lname, SORT_BY_NAME_FILENAME, maxlen) == 0 || strncmp(lname, SORT_BY_NAME_NAME, maxlen) == 0) {
        return FILENAME;
    }
    if (strncmp(lname, SORT_BY_NAME_FILESIZE, maxlen) == 0 || strncmp(lname, SORT_BY_NAME_SIZE, maxlen) == 0) {
        return FILESIZE;
    }
    if (strncmp(lname, SORT_BY_NAME_FILETYPE, maxlen) == 0 || strncmp(lname, SORT_BY_NAME_TYPE, maxlen) == 0) {
        return FILETYPE;
    }
    if (strncmp(lname, SORT_BY_NAME_LASTMOD, maxlen) == 0) {
        return LASTMOD;
    }
    return FILEPATH;
}

void sort_by_to_name(const SortBy sort_by, char *name)
{
    switch(sort_by) {
        case FILEPATH:
            strncpy(name, SORT_BY_NAME_FILEPATH, 8);
            name[8] = '\0';
            break;
        case FILENAME:
            strncpy(name, SORT_BY_NAME_FILENAME, 8);
            name[8] = '\0';
            break;
        case FILESIZE:
            strncpy(name, SORT_BY_NAME_FILESIZE, 8);
            name[8] = '\0';
            break;
        case FILETYPE:
            strncpy(name, SORT_BY_NAME_FILETYPE, 8);
            name[8] = '\0';
            break;
        case LASTMOD:
            strncpy(name, SORT_BY_NAME_LASTMOD, 7);
            name[7] = '\0';
            break;
        default:
            strncpy(name, SORT_BY_NAME_UNKNOWN, 7);
            name[7] = '\0';
    }
}
