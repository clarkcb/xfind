#ifndef FINDERR_H
#define FINDERR_H

/*
 * Error codes - other than E_OK, these start at >300 to be well above the values in
 *               errno.h
 */
#define E_OK                               0 /* OK (no error) */
#define E_UNKNOWN_ERROR                  301 /* Unknown error */
#define E_STARTPATH_NOT_DEFINED          302 /* Startpath not defined */
#define E_STARTPATH_NOT_FOUND            303 /* Startpath not found */
#define E_STARTPATH_STAT_FAILED          304 /* Unable to stat startpath */
#define E_STARTPATH_NON_MATCHING         305 /* Startpath does not match search criteria */
#define E_STARTPATH_UNSUPPORTED_FILETYPE 306 /* Startpath is an unsupported file type */
#define E_INVALID_OPTION                 307 /* Invalid option */
#define E_INVALID_ARG                    308 /* Invalid arg */
#define E_MISSING_ARG_FOR_OPTION         309 /* Missing arg for arg option */
#define E_DIRECTORY_NOT_FOUND            310 /* Directory not found */
#define E_FILE_NOT_FOUND                 311 /* File not found */
#define E_FILENAME_TOO_LONG              312 /* Filename is too long */
#define E_INVALID_DATESTRING             313 /* Invalid date string (for max_last_mod/min_last_mod) */

typedef unsigned int error_t;

void handle_error(error_t err);

#endif
