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
#define E_STARTPATH_NOT_READABLE         304 /* Startpath not readable */
#define E_STARTPATH_STAT_FAILED          305 /* Unable to stat startpath */
#define E_STARTPATH_NON_MATCHING         306 /* Startpath does not match search criteria */
#define E_STARTPATH_UNSUPPORTED_FILETYPE 307 /* Startpath is an unsupported file type */
#define E_INVALID_OPTION                 308 /* Invalid option */
#define E_INVALID_ARG                    309 /* Invalid arg */
#define E_INVALID_ARG_FOR_OPTION         310 /* Missing arg for arg option */
#define E_MISSING_ARG_FOR_OPTION         311 /* Missing arg for arg option */
#define E_DIRECTORY_NOT_FOUND            312 /* Directory not found */
#define E_FILE_NOT_FOUND                 313 /* File not found */
#define E_FILENAME_TOO_LONG              314 /* Filename is too long */
#define E_INVALID_DATESTRING             315 /* Invalid date string (for max_last_mod/min_last_mod) */
#define E_INVALID_DEPTH_RANGE            316 /* Invalid depth range (max_depth < min_depth) */
#define E_INVALID_LASTMOD_RANGE          317 /* Invalid lastmod range (max_last_mod < min_last_mod) */
#define E_INVALID_SIZE_RANGE             318 /* Invalid size range (max_size < min_size) */
#define E_JSON_PARSE_ERROR               319 /* JSON parsing error (invalid on non-JSON) */

typedef unsigned int error_t;

void handle_error(error_t err);

#endif
