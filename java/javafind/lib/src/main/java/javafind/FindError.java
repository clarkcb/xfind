package javafind;

public enum FindError {
    STARTPATH_NOT_DEFINED("Startpath not defined"),
    STARTPATH_NOT_FOUND("Startpath not found"),
    STARTPATH_NOT_READABLE("Startpath not readable"),
    INVALID_RANGE_FOR_MINDEPTH_AND_MAXDEPTH("Invalid range for mindepth and maxdepth"),
    INVALID_RANGE_FOR_MINLASTMOD_AND_MAXLASTMOD("Invalid range for minlastmod and maxlastmod"),
    INVALID_RANGE_FOR_MINSIZE_AND_MAXSIZE("Invalid range for minsize and maxsize"),
    STARTPATH_DOES_NOT_MATCH_FIND_SETTINGS("Startpath does not match find settings");

    private final String message;

    FindError(final String message) {
        this.message = message;
    }

    String getMessage() {
        return message;
    }
}
