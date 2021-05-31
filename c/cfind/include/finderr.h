#ifndef FINDERR_H
#define FINDERR_H

#define E_OK                              0 /* OK (no error) */
#define E_UNKNOWN_ERROR                   1 /* Unknown error */
#define E_STARTPATH_NOT_DEFINED           2 /* Startpath not defined */
#define E_STARTPATH_NOT_FOUND             3 /* Startpath not found */
#define E_STARTPATH_STAT_FAILED           4 /* Unable to stat startpath */
#define E_STARTPATH_NON_MATCHING          5 /* Startpath does not match search criteria */
#define E_STARTPATH_UNSUPPORTED_FILETYPE  6 /* Startpath is an unsupported file type */
#define E_INVALID_OPTION                  7 /* Invalid option */
#define E_INVALID_ARG                     8 /* Invalid arg */
#define E_MISSING_ARG_FOR_OPTION          9 /* Missing arg for arg option */
#define E_DIRECTORY_NOT_FOUND            10 /* Directory not found */
#define E_FILENAME_TOO_LONG              11 /* Filename is too long */


void handle_error(int errnum);

#endif
