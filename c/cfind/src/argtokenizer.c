#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "argtokenizer.h"

#include <stdlib.h>

#include "common.h"
#include "fileutil.h"

error_t tokenize_args(const int argc, char *argv[], Options *options, ArgTokenNode *arg_token_node)
{
    int i = 0;
    while (i < argc) {
        const size_t arg_len = strnlen(argv[i], MAX_STRING_LENGTH);
        if (arg_len < 1) {
            return E_INVALID_ARG;
        }
        if (argv[i][0] == '-') {
            unsigned int c = 1;
            while (c < arg_len && argv[i][c] == '-') {
                c++;
            }
            if (c == arg_len) {
                return E_INVALID_ARG;
            }

            // Adjust arg_name in case it includes '=' + val
            size_t arg_name_len = arg_len - c + 1;
            const char *eq_ptr = strchr(argv[i], '=');
            if (eq_ptr != NULL) {
                arg_name_len = eq_ptr - argv[i] - c;
            }
            char arg_name[arg_name_len];
            strncpy(arg_name, argv[i] + c, arg_name_len);
            arg_name[arg_name_len] = '\0';

            // If c == 2 then it's a long arg, else short arg
            if (c == 2) {
                const Option *o = find_option_for_long_arg(arg_name, options);
                if (o == NULL || o->arg_type == ARG_TOKEN_TYPE_UNKNOWN) {
                    return E_INVALID_ARG;
                }
                if (o->arg_type == ARG_TOKEN_TYPE_BOOL) {
                    ArgToken *a = new_bool_arg_token(arg_name, 1);
                    add_arg_token_to_arg_token_node(a, arg_token_node);
                    i++;
                } else {
                    // If eq_ptr != NULL, get arg_val from end of argv[i], else from argv[i+1]
                    size_t arg_val_len = 0;
                    char *arg_val = NULL;
                    if (eq_ptr != NULL) {
                        arg_val_len = strnlen(eq_ptr, MAX_STRING_LENGTH);
                        char argv_val[arg_val_len];
                        strncpy(argv_val, eq_ptr + 1, arg_val_len);
                        argv_val[arg_val_len] = '\0';
                        arg_val = strdup(argv_val);
                        i += 1;
                    } else if (i < argc - 1) {
                        arg_val_len = strnlen(argv[i+1], MAX_STRING_LENGTH);
                        if (arg_val_len > 0) {
                            arg_val = strdup(argv[i+1]);
                            i += 2;
                        } else {
                            return E_INVALID_ARG;
                        }
                    } else {
                        return E_MISSING_ARG_FOR_OPTION;
                    }

                    // check other options types
                    if (o->arg_type == ARG_TOKEN_TYPE_STR) {
                        if (arg_val_len == 0) return E_MISSING_ARG_FOR_OPTION;
                        ArgToken *a = new_str_arg_token(arg_name, arg_val);
                        add_arg_token_to_arg_token_node(a, arg_token_node);
                    } else if (o->arg_type == ARG_TOKEN_TYPE_INT) {
                        if (arg_val_len == 0) return E_MISSING_ARG_FOR_OPTION;
                        char *end_ptr = NULL;
                        const int int_val = (int)strtol(arg_val, &end_ptr, 10);
                        ArgToken *a = new_int_arg_token(arg_name, int_val);
                        add_arg_token_to_arg_token_node(a, arg_token_node);
                    } else if (o->arg_type == ARG_TOKEN_TYPE_LONG) {
                        if (arg_val_len == 0) return E_MISSING_ARG_FOR_OPTION;
                        char *end_ptr = NULL;
                        const long long_val = strtol(arg_val, &end_ptr, 10);
                        ArgToken *a = new_long_arg_token(arg_name, long_val);
                        add_arg_token_to_arg_token_node(a, arg_token_node);
                    } else {
                        return E_INVALID_OPTION;
                    }
                }
            } else {
                // Process short args, each char separately
                for (int j = 1; j < arg_name_len; j++) {
                    char short_arg[2];
                    short_arg[0] = argv[i][j];
                    short_arg[1] = '\0';

                    const Option *o = find_option_for_short_arg(short_arg, options);
                    if (o == NULL || o->arg_type == ARG_TOKEN_TYPE_UNKNOWN) {
                        return E_INVALID_ARG;
                    }
                    if (o->arg_type == ARG_TOKEN_TYPE_BOOL) {
                        ArgToken *a = new_bool_arg_token(o->long_arg, 1);
                        add_arg_token_to_arg_token_node(a, arg_token_node);
                    } else {
                        char *arg_val = NULL;
                        if (i < argc - 1) {
                            if (strnlen(argv[i+1], MAX_STRING_LENGTH) > 0) {
                                arg_val = argv[i+1];
                                i++;
                            } else {
                                return E_INVALID_ARG;
                            }
                        } else {
                            return E_MISSING_ARG_FOR_OPTION;
                        }

                        if (o->arg_type == ARG_TOKEN_TYPE_STR) {
                            if (arg_val == NULL) return E_MISSING_ARG_FOR_OPTION;
                            ArgToken *a = new_str_arg_token(o->long_arg, arg_val);
                            add_arg_token_to_arg_token_node(a, arg_token_node);
                        } else if (o->arg_type == ARG_TOKEN_TYPE_INT) {
                            if (arg_val == NULL) return E_MISSING_ARG_FOR_OPTION;
                            char *end_ptr = NULL;
                            const int int_val = (int)strtol(arg_val, &end_ptr, 10);
                            ArgToken *a = new_int_arg_token(o->long_arg, int_val);
                            add_arg_token_to_arg_token_node(a, arg_token_node);
                        } else if (o->arg_type == ARG_TOKEN_TYPE_LONG) {
                            if (arg_val == NULL) return E_MISSING_ARG_FOR_OPTION;
                            char *end_ptr = NULL;
                            const long long_val = strtol(arg_val, &end_ptr, 10);
                            ArgToken *a = new_long_arg_token(o->long_arg, long_val);
                            add_arg_token_to_arg_token_node(a, arg_token_node);
                        } else {
                            return E_INVALID_OPTION;
                        }
                    }
                }
                i++;
            }
        } else {
            ArgToken *a = new_str_arg_token("path", argv[i]);
            add_arg_token_to_arg_token_node(a, arg_token_node);
            i++;
        }
    }
    return E_OK;
}

error_t tokenize_json_obj(const cJSON *settings_json, Options *options, ArgTokenNode *arg_token_node)
{
    const cJSON *setting_json = NULL;

    cJSON_ArrayForEach(setting_json, settings_json) {
        const Option *o = find_option_for_long_arg(setting_json->string, options);
        if (o == NULL || o->arg_type == ARG_TOKEN_TYPE_UNKNOWN) {
            return E_INVALID_ARG;
        }
        if (o->arg_type == ARG_TOKEN_TYPE_BOOL) {
            if (cJSON_IsBool(setting_json)) {
                const int flag = cJSON_IsTrue(setting_json) ? 1 : 0;
                ArgToken *a = new_bool_arg_token(setting_json->string, flag);
                add_arg_token_to_arg_token_node(a, arg_token_node);
            } else {
                return E_INVALID_ARG_FOR_OPTION;
            }
        } else if (o->arg_type == ARG_TOKEN_TYPE_STR) {
            if (cJSON_IsString(setting_json) && setting_json->valuestring != NULL) {
                ArgToken *a = new_str_arg_token(setting_json->string, setting_json->valuestring);
                add_arg_token_to_arg_token_node(a, arg_token_node);
            } else if (cJSON_IsArray(setting_json)) {
                // Add each element of array
                const cJSON *elem_json = NULL;
                cJSON_ArrayForEach(elem_json, setting_json) {
                    if (cJSON_IsString(elem_json) && elem_json->valuestring != NULL) {
                        ArgToken *a = new_str_arg_token(setting_json->string, elem_json->valuestring);
                        add_arg_token_to_arg_token_node(a, arg_token_node);
                    }
                }
            } else {
                return E_INVALID_ARG_FOR_OPTION;
            }
        } else if (o->arg_type == ARG_TOKEN_TYPE_INT) {
            if (cJSON_IsNumber(setting_json)) {
                ArgToken *a = new_int_arg_token(setting_json->string, setting_json->valueint);
                add_arg_token_to_arg_token_node(a, arg_token_node);
            } else {
                return E_INVALID_ARG_FOR_OPTION;
            }
        } else if (o->arg_type == ARG_TOKEN_TYPE_LONG) {
            if (cJSON_IsNumber(setting_json)) {
                ArgToken *a = new_long_arg_token(setting_json->string, setting_json->valueint);
                add_arg_token_to_arg_token_node(a, arg_token_node);
            } else {
                return E_INVALID_ARG_FOR_OPTION;
            }
        } else {
            return E_INVALID_OPTION;
        }
    }

    return E_OK;
}

error_t tokenize_json_string(const char *settings_json_str, Options *options, ArgTokenNode *arg_token_node)
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

    err = tokenize_json_obj(settings_json, options, arg_token_node);

    end:
        cJSON_Delete(settings_json);
    return err;
}

error_t tokenize_json_file(const char *json_file_path, Options *options, ArgTokenNode *arg_token_node) {
    error_t err = E_OK;

    // TODO: expand path before checking if exists
    const size_t path_len = strnlen(json_file_path, MAX_PATH_LENGTH);
    char *expanded_path = malloc(path_len * 2 + 1);
    assert(expanded_path != NULL);
    expanded_path[0] = '\0';
    expand_path(json_file_path, &expanded_path);

    if (!dir_or_file_exists(expanded_path)) {
        err = E_FILE_NOT_FOUND;
        free(expanded_path);
        return err;
    }

    // Verify json file (has .json extension)
    const size_t file_path_len = strlen(json_file_path);
    if (file_path_len < 6 || strcmp(json_file_path + file_path_len - 5, ".json") != 0) {
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
        sprintf(err_msg, "Unable to load %s", json_file_path);
        err = E_UNKNOWN_ERROR;
        free(expanded_path);
        return err;
    }

    free(expanded_path);
    return tokenize_json_string(contents, options, arg_token_node);
}
