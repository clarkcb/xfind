#ifndef ARGTOKENNODE_H
#define ARGTOKENNODE_H

#include <stdbool.h>
#include <stdlib.h>

#define ARG_TOKEN_TYPE_BOOL 0
#define ARG_TOKEN_TYPE_STR  1
#define ARG_TOKEN_TYPE_INT  2
#define ARG_TOKEN_TYPE_LONG 3

union TokenValue {
    char *string_val;
    int int_val; // use for bool and int
    long long_val;
};

typedef struct ArgToken {
    char *name;
    int token_type;
    union TokenValue value;
} ArgToken;

typedef struct ArgTokenNode {
    ArgToken *token;
    struct ArgTokenNode *next;
} ArgTokenNode;

ArgToken *new_arg_token(const char *name, int token_type, union TokenValue value);

ArgToken *new_str_arg_token(const char *name, const char *str_val);

ArgToken *new_bool_arg_token(const char *name, int bool_val);

ArgToken *new_int_arg_token(const char *name, int int_val);

ArgToken *new_long_arg_token(const char *name, long long_val);

void destroy_arg_token(ArgToken *a);

ArgTokenNode *empty_arg_token_node(void);

ArgTokenNode *new_arg_token_node(ArgToken *a);

void add_arg_token_to_arg_token_node(ArgToken *a, ArgTokenNode *arg_token_node);

bool is_null_or_empty_arg_token_node(const ArgTokenNode *arg_token_node);

size_t arg_token_node_count(ArgTokenNode *arg_token_node);

void destroy_arg_token_node(ArgTokenNode *arg_token_node);

#endif // ARGTOKENNODE_H
