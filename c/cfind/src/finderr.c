#include "common.h"
#include "finderr.h"

void handle_error(error_t err)
{
    switch (err) {
    case E_STARTPATH_NOT_DEFINED:
        log_err("Startpath not defined");
        break;
    case E_STARTPATH_NOT_FOUND:
        log_err("Startpath not found");
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
    case E_MISSING_ARG_FOR_OPTION:
        log_err("Missing arg for option");
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
    default:
        log_err("Unknown error occurred");
        break;
    }
}
