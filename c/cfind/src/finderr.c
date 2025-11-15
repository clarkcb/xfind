#include "common.h"
#include "errno.h"
#include "finderr.h"

#include <string.h>

void get_error_message(const error_t err, char *err_msg)
{
    char err_buf[100];
    err_buf[0] = '\0';
    switch (err) {
    case EPERM:
        strncpy(err_buf, "Operation not permitted", 100);
        break;
    case ENOENT:
        strncpy(err_buf, "No such file or directory", 100);
        break;
    case ESRCH:
        strncpy(err_buf, "No such process", 100);
        break;
    case EINTR:
        strncpy(err_buf, "Interrupted system call", 100);
        break;
    case EIO:
        strncpy(err_buf, "Input/output error", 100);
        break;
    case ENXIO:
        strncpy(err_buf, "Device not configured", 100);
        break;
    case E2BIG:
        strncpy(err_buf, "Argument list too long", 100);
        break;
    case ENOEXEC:
        strncpy(err_buf, "Exec format error", 100);
        break;
    case EBADF:
        strncpy(err_buf, "Bad file descriptor", 100);
        break;
    case ECHILD:
        strncpy(err_buf, "No child processes", 100);
        break;
    case EDEADLK:
        strncpy(err_buf, "Resource deadlock avoided", 100);
        break;
    case ENOMEM:
        strncpy(err_buf, "Cannot allocate memory", 100);
        break;
    case EACCES:
        strncpy(err_buf, "Permission denied", 100);
        break;
    case EFAULT:
        strncpy(err_buf, "Bad address", 100);
        break;
    case EBUSY:
        strncpy(err_buf, "Device / Resource busy", 100);
        break;
    case EEXIST:
        strncpy(err_buf, "File exists", 100);
        break;
    case EXDEV:
        strncpy(err_buf, "Cross-device link", 100);
        break;
    case ENODEV:
        strncpy(err_buf, "Operation not supported by device", 100);
        break;
    case ENOTDIR:
        strncpy(err_buf, "Not a directory", 100);
        break;
    case EISDIR:
        strncpy(err_buf, "Is a directory", 100);
        break;
    case EINVAL:
        strncpy(err_buf, "Invalid argument", 100);
        break;
    case ENFILE:
        strncpy(err_buf, "Too many open files in system", 100);
        break;
    case EMFILE:
        strncpy(err_buf, "Too many open files", 100);
        break;
    case ENOTTY:
        strncpy(err_buf, "Inappropriate ioctl for device", 100);
        break;
    case ETXTBSY:
        strncpy(err_buf, "Text file busy", 100);
        break;
    case EFBIG:
        strncpy(err_buf, "File too large", 100);
        break;
    case ENOSPC:
        strncpy(err_buf, "No space left on device", 100);
        break;
    case ESPIPE:
        strncpy(err_buf, "Illegal seek", 100);
        break;
    case EROFS:
        strncpy(err_buf, "Read-only file system", 100);
        break;
    case EMLINK:
        strncpy(err_buf, "Too many links", 100);
        break;
    case EPIPE:
        strncpy(err_buf, "Broken pipe", 100);
        break;
    case E_STARTPATH_NOT_DEFINED:
        strncpy(err_buf, "Startpath not defined", 100);
        break;
    case E_STARTPATH_NOT_FOUND:
        strncpy(err_buf, "Startpath not found", 100);
        break;
    case E_STARTPATH_NOT_READABLE:
        strncpy(err_buf, "Startpath not readable", 100);
        break;
    case E_STARTPATH_STAT_FAILED:
        strncpy(err_buf, "An unknown error occurred trying to read startpath", 100);
        break;
    case E_STARTPATH_NON_MATCHING:
        strncpy(err_buf, "Startpath does not match find criteria", 100);
        break;
    case E_STARTPATH_UNSUPPORTED_FILETYPE:
        strncpy(err_buf, "Startpath is an unsupported file type", 100);
        break;
    case E_INVALID_OPTION:
        strncpy(err_buf, "Invalid option", 100);
        break;
    case E_INVALID_ARG:
        strncpy(err_buf, "Invalid arg", 100);
        break;
    case E_INVALID_ARG_FOR_OPTION:
            strncpy(err_buf, "Invalid value for option", 100);
        break;
    case E_MISSING_ARG_FOR_OPTION:
        strncpy(err_buf, "Missing value for option", 100);
        break;
    case E_DIRECTORY_NOT_FOUND:
        strncpy(err_buf, "Directory not found", 100);
        break;
    case E_FILE_NOT_FOUND:
        strncpy(err_buf, "File not found", 100);
        break;
    case E_FILENAME_TOO_LONG:
        strncpy(err_buf, "Filename is too long", 100);
        break;
    case E_INVALID_DATESTRING:
        strncpy(err_buf, "Invalid date string", 100);
        break;
    case E_INVALID_DEPTH_RANGE:
        strncpy(err_buf, "Invalid range for mindepth and maxdepth", 100);
        break;
    case E_INVALID_LASTMOD_RANGE:
        strncpy(err_buf, "Invalid range for minlastmod and maxlastmod", 100);
        break;
    case E_INVALID_SIZE_RANGE:
        strncpy(err_buf, "Invalid range for minsize and maxsize", 100);
        break;
    case E_JSON_PARSE_ERROR:
        strncpy(err_buf, "Unable to parse JSON", 100);
        break;
    default:
        strncpy(err_buf, "Unknown error occurred", 100);
        break;
    }
    size_t eb_len = strnlen(err_buf, 100);
    strncpy(err_msg, err_buf, eb_len);
    err_msg[eb_len] = '\0';
}

void handle_error(const error_t err)
{
    char err_msg[100];
    err_msg[0] = '\0';
    get_error_message(err, err_msg);
    log_err(err_msg);
}
