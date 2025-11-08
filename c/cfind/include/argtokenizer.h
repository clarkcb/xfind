#ifndef ARGTOKENIZER_H
#define ARGTOKENIZER_H

#include <cjson/cJSON.h>

#include "argtokennode.h"
#include "finderr.h"
#include "options.h"

error_t tokenize_args(int argc, char *argv[], Options *options, ArgTokenNode *arg_token_node);
error_t tokenize_json_obj(const cJSON *settings_json, Options *options, ArgTokenNode *arg_token_node);
error_t tokenize_json_string(const char *settings_json_str, Options *options, ArgTokenNode *arg_token_node);
error_t tokenize_json_file(const char *json_file_path, Options *options, ArgTokenNode *arg_token_node);

#endif // ARGTOKENIZER_H
