#include <stdlib.h>
#include <string.h>

#include "config.h"

void get_homepath(char *dest)
{
    char *homepath = getenv("HOME");
    if (dest == NULL || homepath == NULL) {
        return;
    }
    dest[0] = '\0';
    strcat(dest, homepath);
    dest[strlen(homepath)] = '\0';
}

void get_xfindpath(char *dest)
{
    if (dest == NULL) {
        return;
    }
    dest[0] = '\0';
    char *xfindpath = getenv("XFIND_PATH");
    if (xfindpath == NULL) {
        char *homepath[MAX_HOMEPATH_LENGTH];
        get_homepath(homepath);
        strcat(dest, homepath);
        strcat(dest, "/src/xfind");
    } else {
        strcat(dest, xfindpath);
    }
}
