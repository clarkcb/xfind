#include "common.h"
#include "errno.h"
#include "finderr.h"

void handle_error(error_t err)
{
    switch (err) {
    case EPERM:
        log_err("Operation not permitted");
        break;
    case ENOENT:
        log_err("No such file or directory");
        break;
    case ESRCH:
        log_err("No such process");
        break;
    case EINTR:
        log_err("Interrupted system call");
        break;
    case EIO:
        log_err("Input/output error");
        break;
    case ENXIO:
        log_err("Device not configured");
        break;
    case E2BIG:
        log_err("Argument list too long");
        break;
    case ENOEXEC:
        log_err("Exec format error");
        break;
    case EBADF:
        log_err("Bad file descriptor");
        break;
    case ECHILD:
        log_err("No child processes");
        break;
    case EDEADLK:
        log_err("Resource deadlock avoided");
        break;
    case ENOMEM:
        log_err("Cannot allocate memory");
        break;
    case EACCES:
        log_err("Permission denied");
        break;
    case EFAULT:
        log_err("Bad address");
        break;
    case EBUSY:
        log_err("Device / Resource busy");
        break;
    case EEXIST:
        log_err("File exists");
        break;
    case EXDEV:
        log_err("Cross-device link");
        break;
    case ENODEV:
        log_err("Operation not supported by device");
        break;
    case ENOTDIR:
        log_err("Not a directory");
        break;
    case EISDIR:
        log_err("Is a directory");
        break;
    case EINVAL:
        log_err("Invalid argument");
        break;
    case ENFILE:
        log_err("Too many open files in system");
        break;
    case EMFILE:
        log_err("Too many open files");
        break;
    case ENOTTY:
        log_err("Inappropriate ioctl for device");
        break;
    case ETXTBSY:
        log_err("Text file busy");
        break;
    case EFBIG:
        log_err("File too large");
        break;
    case ENOSPC:
        log_err("No space left on device");
        break;
    case ESPIPE:
        log_err("Illegal seek");
        break;
    case EROFS:
        log_err("Read-only file system");
        break;
    case EMLINK:
        log_err("Too many links");
        break;
    case EPIPE:
        log_err("Broken pipe");
        break;
    case E_STARTPATH_NOT_DEFINED:
        log_err("Startpath not defined");
        break;
    case E_STARTPATH_NOT_FOUND:
        log_err("Startpath not found");
        break;
    case E_STARTPATH_NOT_READABLE:
        log_err("Startpath not readable");
        break;
    case E_STARTPATH_STAT_FAILED:
        log_err("An unknown error occurred trying to read startpath");
        break;
    case E_STARTPATH_NON_MATCHING:
        log_err("Startpath does not match find criteria");
        break;
    case E_STARTPATH_UNSUPPORTED_FILETYPE:
        log_err("Startpath is an unsupported file type");
        break;
    case E_INVALID_OPTION:
        log_err("Invalid option");
        break;
    case E_INVALID_ARG:
        log_err("Invalid arg");
        break;
    case E_INVALID_ARG_FOR_OPTION:
            log_err("Invalid value for option");
        break;
    case E_MISSING_ARG_FOR_OPTION:
        log_err("Missing value for option");
        break;
    case E_DIRECTORY_NOT_FOUND:
        log_err("Directory not found");
        break;
    case E_FILE_NOT_FOUND:
        log_err("File not found");
        break;
    case E_FILENAME_TOO_LONG:
        log_err("Filename is too long");
        break;
    case E_INVALID_DATESTRING:
        log_err("Invalid date string");
        break;
    case E_INVALID_DEPTH_RANGE:
        log_err("Invalid range for mindepth and maxdepth");
        break;
    case E_INVALID_LASTMOD_RANGE:
        log_err("Invalid range for minlastmod and maxlastmod");
        break;
    case E_INVALID_SIZE_RANGE:
        log_err("Invalid range for minsize and maxsize");
        break;
    case E_JSON_PARSE_ERROR:
        log_err("Unable to parse JSON");
        break;
    default:
        log_err("Unknown error occurred");
        break;
    }
}
