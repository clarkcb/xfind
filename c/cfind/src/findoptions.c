#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <cjson/cJSON.h>

#include "common.h"
#include "config.h"
#include "filetypes.h"
#include "fileutil.h"
#include "findoptions.h"

#define BOOL_OPTION_COUNT 23
const size_t bool_option_count = BOOL_OPTION_COUNT;
char **bool_option_names = (char *[]) {
    "archivesonly",
    "colorize",
    "debug",
    "excludearchives",
    "excludehidden",
    "followsymlinks",
    "includearchives",
    "includehidden",
    "help",
    "nocolorize",
    "nofollowsymlinks",
    "noprintdirs",
    "noprintfiles",
    "norecursive",
    "printdirs",
    "printfiles",
    "recursive",
    "sort-ascending",
    "sort-caseinsensitive",
    "sort-casesensitive",
    "sort-descending",
    "verbose",
    "version"
};

char **bool_option_abbrs = (char *[]) {
    "a", // archivesonly
    "c", // colorize
    "",  // debug
    "Z", // excludearchives
    "",  // excludehidden
    "",  // followsymlinks
    "z", // includearchives
    "",  // includehidden
    "h", // help
    "C", // nocolorize
    "",  // nofollowsymlinks
    "",  // noprintdirs
    "",  // noprintfiles
    "R", // norecursive
    "",  // printdirs
    "",  // printfiles
    "r", // recursive
    "",  // sort-ascending
    "",  // sort-caseinsensitive
    "",  // sort-casesensitive
    "",  // sort-descending
    "v", // verbose
    "V"  // version
};

#define STRING_OPTION_COUNT 17
const size_t string_option_count = STRING_OPTION_COUNT;
char **string_option_names = (char *[]) {
    "in-archiveext",
    "in-archivefilepattern",
    "in-dirpattern",
    "in-ext",
    "in-filepattern",
    "in-filetype",
    "maxlastmod",
    "minlastmod",
    "out-archiveext",
    "out-archivefilepattern",
    "out-dirpattern",
    "out-ext",
    "out-filepattern",
    "out-filetype",
    "path",
    "settings-file",
    "sort-by",
};
char **string_option_abbrs = (char *[]) {
    "",  // in-archiveext
    "",  // in-archivefilepattern
    "d", // in-dirpattern
    "x", // in-ext
    "f", // in-filepattern
    "t", // in-filetype
    "",  // maxlastmod
    "",  // minlastmod
    "",  // out-archiveext
    "",  // out-archivefilepattern
    "D", // out-dirpattern
    "X", // out-ext
    "F", // out-filepattern
    "T", // out-filetype
    "",  // path
    "",  // settings-file
    ""   // sort-by
};

#define INT_OPTION_COUNT 2
const size_t int_option_count = INT_OPTION_COUNT;
char **int_option_names = (char *[]) {
    "maxdepth",
    "mindepth"
};
char **int_option_abbrs = (char *[]) {
    "",  // maxdepth
    ""   // mindepth
};

#define LONG_OPTION_COUNT 2
const size_t long_option_count = LONG_OPTION_COUNT;
char **long_option_names = (char *[]) {
    "maxsize",
    "minsize"
};
char **long_option_abbrs = (char *[]) {
    "",  // maxsize
    ""   // minsize
};

FindOption *new_find_option(const char *long_arg, const char *short_arg, const char *desc)
{
    FindOption *o = malloc(sizeof(FindOption));
    assert(o != NULL);
    o->long_arg = long_arg;
    o->short_arg = short_arg;
    o->description = desc;
    return o;
}

FindOptions *empty_find_options(void)
{
    FindOptions *options = malloc(sizeof(FindOptions));
    options->option = NULL;
    options->next = NULL;
    return options;
}

FindOptions *new_find_options(FindOption *o)
{
    FindOptions *options = malloc(sizeof(FindOptions));
    options->option = o;
    options->next = NULL;
    return options;
}

void add_to_find_options(FindOption *o, FindOptions *options)
{
    if (options->option == NULL) {
        options->option = o;
    } else {
        FindOptions *temp = options;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_find_options(o);
    }
}

static error_t parse_find_options(const char * const findoptions_json_str, FindOptions *options)
{
    const cJSON *findoption_json = NULL;
    const cJSON *findoptions_json = NULL;

    error_t err = E_OK;

    cJSON *file_json = cJSON_Parse(findoptions_json_str);
    if (file_json == NULL || cJSON_IsInvalid(file_json)) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Error before: %s\n", error_ptr);
        }
        err = E_UNKNOWN_ERROR;
        goto end;
    }

    findoptions_json = cJSON_GetObjectItemCaseSensitive(file_json, "findoptions");
    cJSON_ArrayForEach(findoption_json, findoptions_json) {
        const cJSON *long_json = NULL;
        const cJSON *short_json = NULL;
        const cJSON *desc_json = NULL;

        long_json = cJSON_GetObjectItemCaseSensitive(findoption_json, "long");
        short_json = cJSON_GetObjectItemCaseSensitive(findoption_json, "short");
        desc_json = cJSON_GetObjectItemCaseSensitive(findoption_json, "desc");
        if ((cJSON_IsString(long_json) && (long_json->valuestring != NULL))
             && (cJSON_IsString(desc_json) && (desc_json->valuestring != NULL))) {

            size_t long_len = strnlen(long_json->valuestring, 100);
            char *longarg = malloc((long_len + 1) * sizeof(char));
            strncpy(longarg, long_json->valuestring, long_len);
            longarg[long_len] = '\0';

            size_t desc_len = strnlen(desc_json->valuestring, 1024);
            char *desc = malloc((desc_len + 1) * sizeof(char));
            strncpy(desc, desc_json->valuestring, desc_len);
            desc[desc_len] = '\0';

            char *shortarg = NULL;
            if (cJSON_IsString(short_json) && (short_json->valuestring != NULL)) {
                shortarg = malloc(2);
                strncpy(shortarg, short_json->valuestring, 1);
                shortarg[1] = '\0';
            }

            FindOption *o = new_find_option(longarg, shortarg, desc);
            add_to_find_options(o, options);
        }
    }

end:
    cJSON_Delete(file_json);
    return err;
}

error_t get_find_options(FindOptions *options)
{
    error_t err = E_OK;

    size_t maxlen = MAX_HOMEPATH_LENGTH + 21;
    char *full_path = malloc(maxlen * sizeof(char));
    get_find_options_path(full_path);

    assert(full_path != NULL);

    if (!dir_or_file_exists(full_path)) {
        err = E_FILE_NOT_FOUND;
        free(full_path);
        return err;
    }

    // load the file
    const long fsize = file_size(full_path);
    // current size is 4732, make sure it's not dramatically bigger than that
    assert(fsize <= 5200);
    char contents[fsize];
    contents[0] = '\0';
    FILE *fp = fopen(full_path, "r");
    int c;
    if (fp != NULL) {
        while((c = getc(fp)) != EOF) {
            strcat(contents, (char *)&c);
        }
        fclose(fp);
    } else {
        char *errmsg = malloc((16 + strnlen(full_path, maxlen)) * sizeof(char));
        sprintf(errmsg, "Unable to load %s", full_path);
        err = E_UNKNOWN_ERROR;
        free(errmsg);
        free(full_path);
        return err;
    }

    err = parse_find_options(contents, options);

    free(full_path);
    return err;
}

static error_t set_bool_setting(const int bool_idx, const bool bool_val, FindSettings *settings)
{
    switch (bool_idx) {
        case ARCHIVES_ONLY:
            set_archives_only(settings, bool_val);
            break;
        case COLORIZE:
            settings->colorize = bool_val;
            break;
        case DEBUG:
            set_debug(settings, bool_val);
            break;
        case EXCLUDE_ARCHIVES:
            settings->include_archives = !bool_val;
            break;
        case EXCLUDE_HIDDEN:
            settings->include_hidden = !bool_val;
            break;
        case FOLLOW_SYMLINKS:
            settings->follow_symlinks = bool_val;
            break;
        case INCLUDE_ARCHIVES:
            settings->include_archives = bool_val;
            break;
        case INCLUDE_HIDDEN:
            settings->include_hidden = bool_val;
            break;
        case HELP:
            settings->print_usage = bool_val;
            break;
        case NO_COLORIZE:
            settings->colorize = !bool_val;
            break;
        case NO_FOLLOW_SYMLINKS:
            settings->follow_symlinks = !bool_val;
            break;
        case NO_PRINT_DIRS:
            settings->print_dirs = !bool_val;
            break;
        case NO_PRINT_FILES:
            settings->print_files = !bool_val;
            break;
        case NO_RECURSIVE:
            settings->recursive = !bool_val;
            break;
        case PRINT_DIRS:
            settings->print_dirs = bool_val;
            break;
        case PRINT_FILES:
            settings->print_files = bool_val;
            break;
        case RECURSIVE:
            settings->recursive = bool_val;
            break;
        case SORT_ASCENDING:
            settings->sort_descending = !bool_val;
            break;
        case SORT_CASE_INSENSITIVE:
            settings->sort_case_insensitive = bool_val;
            break;
        case SORT_CASE_SENSITIVE:
            settings->sort_case_insensitive = !bool_val;
            break;
        case SORT_DESCENDING:
            settings->sort_descending = bool_val;
            break;
        case VERBOSE:
            settings->verbose = bool_val;
            break;
        case VERSION:
            settings->print_version = bool_val;
            break;
        default:
            return E_INVALID_OPTION;
    }
    return E_OK;
}

static error_t set_string_setting(int str_idx, char *str_val, FindSettings *settings)
{
    switch (str_idx) {
    case IN_ARCHIVE_EXTENSION:
        if (settings->in_archive_extensions == NULL)
            settings->in_archive_extensions = new_string_node_from_char_split(',', str_val);
        else
            add_char_split_to_string_node(',', str_val, settings->in_archive_extensions);
        break;
    case IN_ARCHIVE_FILE_PATTERN:
        if (settings->in_archive_file_patterns == NULL)
            settings->in_archive_file_patterns = new_regex_node_from_string(str_val);
        else
            add_string_to_regex_node(str_val, settings->in_archive_file_patterns);
        break;
    case IN_DIR_PATTERN:
        if (settings->in_dir_patterns == NULL)
            settings->in_dir_patterns = new_regex_node_from_string(str_val);
        else
            add_string_to_regex_node(str_val, settings->in_dir_patterns);
        break;
    case IN_EXTENSION:
        if (settings->in_extensions == NULL)
            settings->in_extensions = new_string_node_from_char_split(',', str_val);
        else
            add_char_split_to_string_node(',', str_val, settings->in_extensions);
        break;
    case IN_FILE_PATTERN:
        if (settings->in_file_patterns == NULL)
            settings->in_file_patterns = new_regex_node_from_string(str_val);
        else
            add_string_to_regex_node(str_val, settings->in_file_patterns);
        break;
    case IN_FILE_TYPE:
        // this is just to wrap in an expression
        if (str_val) {
            if (settings->in_file_types == NULL) {
                settings->in_file_types = empty_int_node();
            }
            FileType file_type = file_type_from_name(str_val);
            int *ftint = malloc(sizeof(int));
            *ftint = (int)file_type;
            add_int_to_int_node(ftint, settings->in_file_types);
        }
        break;
    case MAX_LAST_MOD:
        if (str_val) {
            struct tm tm;
            memset(&tm, 0, sizeof(tm));
            if (strptime(str_val, "%Y-%m-%d", &tm) == NULL) {
                return E_INVALID_DATESTRING;
            } else {
                settings->max_last_mod = mktime(&tm);
            }
        } 
        break;
    case MIN_LAST_MOD:
        if (str_val) {
            struct tm tm;
            memset(&tm, 0, sizeof(tm));
            if (strptime(str_val, "%Y-%m-%d", &tm) == NULL) {
                return E_INVALID_DATESTRING;
            } else {
                settings->min_last_mod = mktime(&tm);
            }
        } 
        break;
    case OUT_ARCHIVE_EXT:
        if (settings->out_archive_extensions == NULL)
            settings->out_archive_extensions = new_string_node_from_char_split(',', str_val);
        else
            add_char_split_to_string_node(',', str_val, settings->out_archive_extensions);
        break;
    case OUT_ARCHIVE_FILE_PATTERN:
        if (settings->out_archive_file_patterns == NULL)
            settings->out_archive_file_patterns = new_regex_node_from_string(str_val);
        else
            add_string_to_regex_node(str_val, settings->out_archive_file_patterns);
        break;
    case OUT_DIR_PATTERN:
        if (settings->out_dir_patterns == NULL)
            settings->out_dir_patterns = new_regex_node_from_string(str_val);
        else
            add_string_to_regex_node(str_val, settings->out_dir_patterns);
        break;
    case OUT_EXTENSION:
        if (settings->out_extensions == NULL)
            settings->out_extensions = new_string_node_from_char_split(',', str_val);
        else
            add_char_split_to_string_node(',', str_val, settings->out_extensions);
        break;
    case OUT_FILE_PATTERN:
        if (settings->out_file_patterns == NULL)
            settings->out_file_patterns = new_regex_node_from_string(str_val);
        else
            add_string_to_regex_node(str_val, settings->out_file_patterns);
        break;
    case OUT_FILE_TYPE:
        // this is just to wrap in an expression
        if (str_val) {
            if (settings->out_file_types == NULL) {
                settings->out_file_types = empty_int_node();
            }
            FileType file_type = file_type_from_name(str_val);
            int *ftint = malloc(sizeof(int));
            *ftint = (int)file_type;
            add_int_to_int_node(ftint, settings->out_file_types);
        }
        break;
    case PATH:
        // this is just to wrap in an expression
        if (str_val) {
            Path *path = new_path(str_val);
            if (settings->paths == NULL) {
                settings->paths = new_path_node(path);
            } else {
                add_path_to_path_node(path, settings->paths);
            }
        }
        break;
    case SETTINGS_FILE:
        // this is just to wrap in an expression
        if (str_val) {
            const error_t err = settings_from_json_file(str_val, settings);
            if (err != E_OK) return err;
        }
        break;
    case SORT_BY:
        // this is just to wrap in an expression
        if (str_val) {
            const SortBy sort_by = sort_by_from_name(str_val);
            settings->sort_by = sort_by;
        }
        break;
    default:
        return E_INVALID_OPTION;
    }
    return E_OK;
}

static error_t set_int_setting(const int int_idx, const int int_val, FindSettings *settings) {
    switch (int_idx) {
        case MAX_DEPTH:
            settings->max_depth = int_val;
            break;
        case MIN_DEPTH:
            settings->min_depth = int_val;
            break;
        default:
            return E_INVALID_OPTION;
    }
    return E_OK;
}

static error_t set_long_setting(const int long_idx, const unsigned long long_val, FindSettings *settings) {
    switch (long_idx) {
        case MAX_SIZE:
            settings->max_size = long_val;
            break;
        case MIN_SIZE:
            settings->min_size = long_val;
            break;
        default:
            return E_INVALID_OPTION;
    }
    return E_OK;
}

error_t settings_from_args(const int argc, char *argv[], FindSettings *settings)
{
    int i = 0;
    settings->print_files = 1;
    while (i < argc) {
        const size_t arglen = strnlen(argv[i], MAX_STRING_LENGTH);
        if (arglen < 1) {
            return E_INVALID_ARG;
        }
        if (argv[i][0] == '-') {
            unsigned int c = 1;
            while (c < arglen && argv[i][c] == '-') {
                c++;
            }
            if (c == arglen) {
                return E_INVALID_ARG;
            }
            char arg_name[arglen - c + 1];
            strncpy(arg_name, argv[i] + c, arglen - c);
            arg_name[arglen - c] = '\0';

            int bool_idx = index_of_string_in_array(arg_name, bool_option_names, bool_option_count);
            if (bool_idx == -1) {
                bool_idx = index_of_string_in_array(arg_name, bool_option_abbrs, bool_option_count);
            }
            if (bool_idx > -1) {
                const error_t e = set_bool_setting(bool_idx, true, settings);
                if (e != E_OK) return e;
                i++;
            } else {
                char *arg_val = NULL;
                if (i < argc - 1) {
                    if (strnlen(argv[i+1], MAX_STRING_LENGTH) > 0) {
                        arg_val = argv[i+1];
                        i += 2;
                    } else {
                        return E_INVALID_ARG;
                    }
                }

                int str_idx = index_of_string_in_array(arg_name, string_option_names, string_option_count);
                if (str_idx == -1) {
                    str_idx = index_of_string_in_array(arg_name, string_option_abbrs, string_option_count);
                }
                if (str_idx > -1) {
                    if (arg_val == NULL) return E_MISSING_ARG_FOR_OPTION;
                    error_t e = set_string_setting(str_idx, arg_val, settings);
                    if (e != E_OK) return e;
                } else {
                    // check int options
                    int int_idx = index_of_string_in_array(arg_name, int_option_names, int_option_count);
                    if (int_idx == -1) {
                        int_idx = index_of_string_in_array(arg_name, int_option_abbrs, int_option_count);
                    }
                    if (int_idx > -1) {
                        if (arg_val == NULL) return E_MISSING_ARG_FOR_OPTION;
                        // const int int_val = atoi(arg_val);
                        char *end_ptr = NULL;
                        const int int_val = (int)strtol(arg_val, &end_ptr, 10);
                        const error_t e = set_int_setting(int_idx, int_val, settings);
                        if (e != E_OK) return e;
                    } else {
                        // check long options
                        int long_idx = index_of_string_in_array(arg_name, long_option_names, long_option_count);
                        if (long_idx == -1) {
                            long_idx = index_of_string_in_array(arg_name, long_option_abbrs, long_option_count);
                        }
                        if (long_idx > -1) {
                            if (arg_val == NULL) return E_MISSING_ARG_FOR_OPTION;
                            char *end_ptr = NULL;
                            const unsigned long long_val = strtoul(arg_val, &end_ptr, 10);
                            const error_t e = set_long_setting(long_idx, long_val, settings);
                            if (e != E_OK) return e;
                        } else {
                            return E_INVALID_OPTION;
                        }
                    }
                }
            }

        } else {
            Path *path = new_path(argv[i]);
            if (settings->paths == NULL) {
                settings->paths = new_path_node(path);
            } else {
                add_path_to_path_node(path, settings->paths);
            }
            i++;
        }
    }
    return E_OK;
}

error_t settings_from_json_obj(const cJSON *settings_json, FindSettings *settings) {
    const cJSON *setting_json = NULL;

    error_t err = E_OK;

    cJSON_ArrayForEach(setting_json, settings_json) {
        int idx = index_of_string_in_array(setting_json->string, string_option_names, STRING_OPTION_COUNT);
        if (idx > -1) {
            if (cJSON_IsString(setting_json) && setting_json->valuestring != NULL) {
                err = set_string_setting(idx, setting_json->valuestring, settings);
                if (err != E_OK) return err;
            } else if (cJSON_IsArray(setting_json)) {
                // Add each element of array
                const cJSON *elem_json = NULL;
                cJSON_ArrayForEach(elem_json, setting_json) {
                    if (cJSON_IsString(elem_json) && elem_json->valuestring != NULL) {
                        err = set_string_setting(idx, elem_json->valuestring, settings);
                        if (err != E_OK) return err;
                    }
                }
            } else {
                return E_INVALID_ARG_FOR_OPTION;
            }
        } else {
            idx = index_of_string_in_array(setting_json->string, bool_option_names, BOOL_OPTION_COUNT);
            if (idx > -1) {
                if (cJSON_IsBool(setting_json)) {
                    const int flag = cJSON_IsTrue(setting_json) ? 1 : 0;
                    err = set_bool_setting(idx, flag, settings);
                    if (err != E_OK) return err;
                } else {
                    return E_INVALID_ARG_FOR_OPTION;
                }
            } else {
                idx = index_of_string_in_array(setting_json->string, int_option_names, INT_OPTION_COUNT);
                if (idx > -1) {
                    if (cJSON_IsNumber(setting_json)) {
                        err = set_int_setting(idx, setting_json->valueint, settings);
                        if (err != E_OK) return err;
                    } else {
                        return E_INVALID_ARG_FOR_OPTION;
                    }
                } else {
                    idx = index_of_string_in_array(setting_json->string, long_option_names, LONG_OPTION_COUNT);
                    if (idx > -1) {
                        if (cJSON_IsNumber(setting_json)) {
                            err = set_long_setting(idx, setting_json->valueint, settings);
                            if (err != E_OK) return err;
                        } else {
                            return E_INVALID_ARG_FOR_OPTION;
                        }
                    }
                }
            }
        }
    }

    return err;
}

error_t settings_from_json_string(const char *settings_json_str, FindSettings *settings)
{
    cJSON *settings_json = NULL;

    error_t err = E_OK;

    settings_json = cJSON_Parse(settings_json_str);
    if (settings_json == NULL || cJSON_IsInvalid(settings_json)) {
        // const char *error_ptr = cJSON_GetErrorPtr();
        // if (error_ptr != NULL) {
        //     fprintf(stderr, "Error before: %s\n", error_ptr);
        // }
        err = E_JSON_PARSE_ERROR;
        goto end;
    }

    // Verify that settings_json is an object
    if (!cJSON_IsObject(settings_json)) {
        err = E_INVALID_ARG;
        goto end;
    }

    err = settings_from_json_obj(settings_json, settings);

end:
    cJSON_Delete(settings_json);
    return err;
}

error_t settings_from_json_file(const char *settings_json_file_path, FindSettings *settings) {
    error_t err = E_OK;

    // TODO: expand path before checking if exists
    const size_t path_len = strnlen(settings_json_file_path, MAX_PATH_LENGTH);
    char *expanded_path = malloc(path_len * 2 + 1);
    expanded_path[0] = '\0';
    expand_path(settings_json_file_path, &expanded_path);

    if (!dir_or_file_exists(expanded_path)) {
        err = E_FILE_NOT_FOUND;
        free(expanded_path);
        return err;
    }

    // Verify json file (has .json extension)
    const size_t file_path_len = strlen(settings_json_file_path);
    if (file_path_len < 6 || strcmp(settings_json_file_path + file_path_len - 5, ".json") != 0) {
        err = E_INVALID_ARG;
        free(expanded_path);
        return err;
    }

    // load the file
    const long fsize = file_size(expanded_path);
    // 5096 would be a big settings file, should be large enough for most cases
    assert(fsize <= 5096);
    char contents[fsize];
    contents[0] = '\0';
    FILE *fp = fopen(expanded_path, "r");
    int c;
    if (fp != NULL) {
        while((c = getc(fp)) != EOF) {
            strcat(contents, (char *)&c);
        }
        fclose(fp);
    } else {
        size_t err_size = 16 + strnlen(expanded_path, MAX_PATH_LENGTH) * sizeof(char);
        char err_msg[err_size];
        err_msg[0] = '\0';
        sprintf(err_msg, "Unable to load %s", settings_json_file_path);
        err = E_UNKNOWN_ERROR;
        free(expanded_path);
        return err;
    }

    free(expanded_path);
    return settings_from_json_string(contents, settings);
}

size_t find_options_count(FindOptions *options)
{
    FindOptions *temp = options;
    size_t optcount = 0;
    while (temp != NULL && temp->option != NULL) {
        optcount++;
        temp = temp->next;
    }
    return optcount;
}

// get the "opt" ("-s,--long") strnlen for the option
static size_t get_option_opt_strlen(const FindOption *o)
{
    // + 2 for leading --
    size_t opt_len = strnlen(o->long_arg, 100) + 2;
    if (o->short_arg != NULL) {
        opt_len += 3;
    }
    return opt_len;
}

static size_t get_longest_opt_strlen(FindOptions *options)
{
    FindOptions *temp = options;
    size_t longest_len = 0;
    while (temp != NULL && temp->option != NULL) {
        // + 2 for leading --
        size_t long_len = strnlen(temp->option->long_arg, 100) + 2;
        if (temp->option->short_arg != NULL) {
            // + 2 for leading - and ,
            long_len += strnlen(temp->option->short_arg, 2) + 1;
        }
        if (long_len > longest_len) {
            longest_len = long_len;
        }
        temp = temp->next;
    }
    return longest_len;
}

static size_t find_option_strlen(FindOption *o)
{
    size_t optlen = strnlen(o->long_arg, 100) + strnlen(o->description, MAX_STRING_LENGTH);
    if (o->short_arg != NULL) {
        optlen += strnlen(o->short_arg, 2);
    }
    return optlen;
}

size_t find_option_usage_strlen(FindOption *o, size_t longest_opt_len)
{
    // + 2 for two spaces between opt and description
    size_t option_len = longest_opt_len + 2 + strnlen(o->description, MAX_STRING_LENGTH);
    return option_len;
}

size_t find_options_usage_strlen(FindOptions *options)
{
    size_t longest_opt_len = get_longest_opt_strlen(options);
    size_t usage_len = 0;
    FindOptions *temp = options;
    while (temp != NULL && temp->option != NULL) {
        // +2 for leading space + \n
        usage_len += find_option_usage_strlen(temp->option, longest_opt_len) + 2;
        temp = temp->next;
    }
    return usage_len;
}

// comparator function for FindOption instances
static int cmp_find_option(const void *a, const void *b)
{
    FindOption **o1 = (FindOption **)a;
    FindOption **o2 = (FindOption **)b;

    size_t o1_len = find_option_strlen(*o1);
    char opt1[o1_len];
    if ((*o1)->short_arg != NULL) {
        const char c1 = tolower((*o1)->short_arg[0]);
        snprintf(opt1, o1_len, "%c@%s", c1, (*o1)->long_arg);
    } else {
        snprintf(opt1, o1_len, "%s", (*o1)->long_arg);
    }

    size_t o2_len = find_option_strlen(*o2);
    char opt2[o2_len];
    if ((*o2)->short_arg != NULL) {
        const char c2 = tolower((*o2)->short_arg[0]);
        snprintf(opt2, o2_len, "%c@%s", c2, (*o2)->long_arg);
    } else {
        snprintf(opt2, o2_len, "%s", (*o2)->long_arg);
    }

    return strcmp(opt1, opt2);
}

// sort a FindOption array
static void sort_find_option_array(FindOption **arr, size_t n)
{
    qsort(arr, n, sizeof(FindOption*), cmp_find_option);
}

void find_options_to_usage_string(FindOptions *options, char *s)
{
    size_t options_count = find_options_count(options);
    FindOption **option_array = malloc(options_count * sizeof(FindOption *));
    FindOptions *temp = options;
    int i = 0;
    while (temp != NULL && temp->option != NULL) {
        option_array[i++] = temp->option;
        temp = temp->next;
    }
    sort_find_option_array(option_array, options_count);

    size_t longest_len = get_longest_opt_strlen(options);
    char line_format[16];
    snprintf(line_format, 16, " %%1$-%ds  %%2$s\n", (int)longest_len);

    for (i = 0; i < options_count; i++) {
        size_t opt_len = get_option_opt_strlen(option_array[i]) + 1;
        char opt_buff[opt_len];
        if (option_array[i]->short_arg != NULL) {
            snprintf(opt_buff, opt_len, "-%s,--%s", option_array[i]->short_arg, option_array[i]->long_arg);
        } else {
            snprintf(opt_buff, opt_len, "--%s", option_array[i]->long_arg);
        }
        opt_buff[opt_len] = '\0';
        // + 5 for three spaces, newline and \0
        size_t line_len = longest_len + strnlen(option_array[i]->description, 1024) + 5;
        char opt_line[line_len];
        snprintf(opt_line, line_len, line_format, opt_buff, option_array[i]->description);
        opt_line[line_len] = '\0';
        strcat(s, opt_line);
    }

    free(option_array);

}

void print_usage(void)
{
    FindOptions *options = empty_find_options();
    error_t err = get_find_options(options);
    if (err) {
        handle_error(err);
    }
    size_t options_len = find_options_usage_strlen(options) + 1;
    char *usage_str = malloc((options_len + 1) * sizeof(char));
    find_options_to_usage_string(options, usage_str);
    log_msg("\nUsage:\n cfind [options] <path> [<path> ...]\n\nOptions:");
    log_msg(usage_str);
    destroy_find_options(options);
    free(usage_str);
}

void destroy_find_option(FindOption *o)
{
    if (o->short_arg != NULL) {
        free(o->short_arg);
    }
    free(o->long_arg);
    free(o->description);
    free(o);
}

void destroy_find_options(FindOptions *options)
{
    if (options != NULL) {
        FindOptions *current = options;
        while (current != NULL) {
            destroy_find_option(current->option);
            FindOptions *next = current->next;
            free(current);
            current = next;
        }
    }
}
