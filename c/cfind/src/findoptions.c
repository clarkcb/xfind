#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <cjson/cJSON.h>

#include "argtokenizer.h"
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

FindOption *new_find_option(const char *long_arg, const char *short_arg, const char *desc, const int arg_type)
{
    return new_option(long_arg, short_arg, desc, arg_type);
}

FindOptions *empty_find_options(void)
{
    return empty_options();
}

FindOptions *new_find_options(FindOption *o)
{
    return new_options(o);
}

void add_to_find_options(FindOption *o, FindOptions *options)
{
    add_to_options(o, options);
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
        if (err != E_OK) {
            goto end;
        }
        const cJSON *long_json = NULL;
        const cJSON *short_json = NULL;
        const cJSON *desc_json = NULL;

        long_json = cJSON_GetObjectItemCaseSensitive(findoption_json, "long");
        short_json = cJSON_GetObjectItemCaseSensitive(findoption_json, "short");
        desc_json = cJSON_GetObjectItemCaseSensitive(findoption_json, "desc");
        if ((cJSON_IsString(long_json) && (long_json->valuestring != NULL))
             && (cJSON_IsString(desc_json) && (desc_json->valuestring != NULL))) {

            size_t long_len = strnlen(long_json->valuestring, 100);
            char *long_arg = malloc((long_len + 1) * sizeof(char));
            assert(long_arg != NULL);
            strncpy(long_arg, long_json->valuestring, long_len);
            long_arg[long_len] = '\0';

            size_t desc_len = strnlen(desc_json->valuestring, 1024);
            char *desc = malloc((desc_len + 1) * sizeof(char));
            assert(desc != NULL);
            strncpy(desc, desc_json->valuestring, desc_len);
            desc[desc_len] = '\0';

            char *short_arg = NULL;
            if (cJSON_IsString(short_json) && (short_json->valuestring != NULL)) {
                short_arg = malloc(2);
                assert(short_arg != NULL);
                strncpy(short_arg, short_json->valuestring, 1);
                short_arg[1] = '\0';
            }

            int arg_type = ARG_TOKEN_TYPE_UNKNOWN;
            const int bool_idx = index_of_string_in_array(long_arg, bool_option_names, bool_option_count);
            if (bool_idx > -1) {
                arg_type = ARG_TOKEN_TYPE_BOOL;
            } else {
                const int str_idx = index_of_string_in_array(long_arg, string_option_names, string_option_count);
                if (str_idx > -1) {
                    arg_type = ARG_TOKEN_TYPE_STR;
                } else {
                    const int int_idx = index_of_string_in_array(long_arg, int_option_names, int_option_count);
                    if (int_idx > -1) {
                        arg_type = ARG_TOKEN_TYPE_INT;
                    } else {
                        const int long_idx = index_of_string_in_array(long_arg, long_option_names, long_option_count);
                        if (long_idx > -1) {
                            arg_type = ARG_TOKEN_TYPE_LONG;
                        } else {
                            err = E_INVALID_OPTION;
                        }
                    }
                }
            }

            FindOption *o = new_find_option(long_arg, short_arg, desc, arg_type);
            add_to_find_options(o, options);
        }
    }

end:
    cJSON_Delete(file_json);
    if (err != E_OK) {
        destroy_find_options(options);
    }
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
    // current size is 5263, make sure it's not dramatically bigger than that
    assert(fsize <= 5300);
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

static error_t set_string_setting(const int str_idx, const char *str_val, FindSettings *settings)
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
            }
            settings->min_last_mod = mktime(&tm);
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

error_t update_settings_from_arg_token_node(const ArgTokenNode *arg_token_node, FindOptions *options, FindSettings *settings)
{
    const ArgTokenNode *temp = arg_token_node;
    while (temp != NULL && temp->token != NULL) {
        if (temp->token->token_type == ARG_TOKEN_TYPE_BOOL) {
            const int bool_idx = index_of_string_in_array(temp->token->name, bool_option_names, bool_option_count);
            if (bool_idx > -1) {
                const error_t e = set_bool_setting(bool_idx, temp->token->value.int_val, settings);
                if (e != E_OK) return e;
            } else {
                return E_INVALID_OPTION;
            }
        } else if (temp->token->token_type == ARG_TOKEN_TYPE_STR) {
            if (strcmp(temp->token->name, "settings-file") == 0) {
                const error_t e = settings_from_json_file(temp->token->value.string_val, options, settings);
                if (e != E_OK) return e;
            } else {
                const int str_idx = index_of_string_in_array(temp->token->name, string_option_names, string_option_count);
                if (str_idx > -1) {
                    const error_t e = set_string_setting(str_idx, temp->token->value.string_val, settings);
                    if (e != E_OK) return e;
                } else {
                    return E_INVALID_OPTION;
                }
            }
        } else if (temp->token->token_type == ARG_TOKEN_TYPE_INT) {
            const int int_idx = index_of_string_in_array(temp->token->name, int_option_names, int_option_count);
            if (int_idx > -1) {
                const error_t e = set_int_setting(int_idx, temp->token->value.int_val, settings);
                if (e != E_OK) return e;
            } else {
                return E_INVALID_OPTION;
            }
        } else if (temp->token->token_type == ARG_TOKEN_TYPE_LONG) {
            const int long_idx = index_of_string_in_array(temp->token->name, long_option_names, long_option_count);
            if (long_idx > -1) {
                const error_t e = set_long_setting(long_idx, temp->token->value.long_val, settings);
                if (e != E_OK) return e;
            } else {
                return E_INVALID_OPTION;
            }
        }
        temp = temp->next;
    }
    return E_OK;
}

error_t settings_from_args(const int argc, char *argv[], FindOptions *options, FindSettings *settings)
{
    ArgTokenNode *arg_token_node = empty_arg_token_node();
    error_t err = tokenize_args(argc, argv, options, arg_token_node);
    if (err != E_OK) return err;
    settings->print_files = 1;
    err = update_settings_from_arg_token_node(arg_token_node, options, settings);
    destroy_arg_token_node(arg_token_node);
    return err;
}

error_t settings_from_json_obj(const cJSON *settings_json, FindOptions *options, FindSettings *settings)
{
    ArgTokenNode *arg_token_node = empty_arg_token_node();
    error_t err = tokenize_json_obj(settings_json, options, arg_token_node);
    if (err != E_OK) return err;
    err = update_settings_from_arg_token_node(arg_token_node, options, settings);
    destroy_arg_token_node(arg_token_node);
    return err;
}

error_t settings_from_json_string(const char *settings_json_str, FindOptions *options, FindSettings *settings)
{
    ArgTokenNode *arg_token_node = empty_arg_token_node();
    error_t err = tokenize_json_string(settings_json_str, options, arg_token_node);
    if (err != E_OK) return err;
    err = update_settings_from_arg_token_node(arg_token_node, options, settings);
    destroy_arg_token_node(arg_token_node);
    return err;
}

error_t settings_from_json_file(const char *settings_json_file_path, FindOptions *options, FindSettings *settings) {
    ArgTokenNode *arg_token_node = empty_arg_token_node();
    error_t err = tokenize_json_file(settings_json_file_path, options, arg_token_node);
    if (err != E_OK) return err;
    err = update_settings_from_arg_token_node(arg_token_node, options, settings);
    destroy_arg_token_node(arg_token_node);
    return err;
}

size_t find_options_count(FindOptions *options)
{
    return options_count(options);
}

// get the "args" ("-s,--long") strnlen for the option
static size_t find_option_args_strlen(const FindOption *o)
{
    // + 2 for leading --
    size_t opt_len = strnlen(o->long_arg, 100) + 2;
    if (o->short_arg != NULL) {
        // 3 for - + short_arg + ,
        opt_len += 3;
    }
    return opt_len;
}

static size_t get_longest_find_option_args_strlen(FindOptions *options)
{
    FindOptions *temp = options;
    size_t longest_len = 0;
    while (temp != NULL && temp->option != NULL) {
        const size_t args_len = find_option_args_strlen(temp->option);
        if (args_len > longest_len) {
            longest_len = args_len;
        }
        temp = temp->next;
    }
    return longest_len;
}

static size_t find_option_strlen(const FindOption *o)
{
    return find_option_args_strlen(o) + strnlen(o->description, MAX_STRING_LENGTH);
}

size_t find_option_usage_strlen(const FindOption *o, const size_t longest_args_len)
{
    return longest_args_len + strnlen(o->description, MAX_STRING_LENGTH);
}

size_t find_options_usage_strlen(FindOptions *options)
{
    const size_t longest_args_len = get_longest_find_option_args_strlen(options);
    size_t usage_len = 0;
    FindOptions *temp = options;
    while (temp != NULL && temp->option != NULL) {
        // +2 for leading space + \n
        usage_len += find_option_usage_strlen(temp->option, longest_args_len) + 2;
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
    assert(arr != NULL);
    qsort(arr, n, sizeof(FindOption*), cmp_find_option);
}

void find_options_to_usage_string(FindOptions *options, char *s)
{
    assert(options != NULL);
    size_t options_count = find_options_count(options);
    FindOption **option_array = malloc(options_count * sizeof(FindOption *));
    assert(option_array != NULL);
    FindOptions *temp = options;
    int i = 0;
    while (temp != NULL && temp->option != NULL) {
        option_array[i++] = temp->option;
        temp = temp->next;
    }
    sort_find_option_array(option_array, options_count);

    const size_t longest_len = get_longest_find_option_args_strlen(options);
    char line_format[16];
    snprintf(line_format, 16, " %%1$-%ds  %%2$s\n", (int)longest_len);

    for (i = 0; i < options_count; i++) {
        size_t opt_len = find_option_strlen(option_array[i]) + 1;
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

void print_usage_for_options(FindOptions *options)
{
    const size_t options_len = find_options_usage_strlen(options) + 1;
    char *usage_str = malloc((options_len + 1) * sizeof(char));
    find_options_to_usage_string(options, usage_str);
    log_msg("\nUsage:\n cfind [options] <path> [<path> ...]\n\nOptions:");
    log_msg(usage_str);
    destroy_find_options(options);
    free(usage_str);
}

void print_usage(void)
{
    FindOptions *options = empty_find_options();
    const error_t err = get_find_options(options);
    if (err) {
        handle_error(err);
        destroy_find_options(options);
        exit(EXIT_FAILURE);
    }
    print_usage_for_options(options);
}

void destroy_find_option(FindOption *o)
{
    destroy_option(o);
}

void destroy_find_options(FindOptions *options)
{
    destroy_options(options);
}
