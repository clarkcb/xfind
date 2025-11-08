#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "argtokennode.h"
#include "common.h"

ArgToken *new_arg_token(const char *name, const int token_type, const union TokenValue value)
{
    ArgToken *a = malloc(sizeof(ArgToken));
    assert(a != NULL);
    a->name = strdup(name);
    a->token_type = token_type;
    a->value = value;
    return a;
}
ArgToken *new_str_arg_token(const char *name, const char *str_val)
{
    ArgToken *a = malloc(sizeof(ArgToken));
    assert(a != NULL);
    a->name = strdup(name);
    a->token_type = ARG_TOKEN_TYPE_STR;
    a->value.string_val = strdup(str_val);
    return a;
}

ArgToken *new_bool_arg_token(const char *name, const int bool_val)
{
    ArgToken *a = malloc(sizeof(ArgToken));
    assert(a != NULL);
    a->name = strdup(name);
    a->token_type = ARG_TOKEN_TYPE_BOOL;
    a->value.int_val = bool_val;
    return a;
}

ArgToken *new_int_arg_token(const char *name, const int int_val)
{
    ArgToken *a = malloc(sizeof(ArgToken));
    assert(a != NULL);
    a->name = strdup(name);
    a->token_type = ARG_TOKEN_TYPE_INT;
    a->value.int_val = int_val;
    return a;
}

ArgToken *new_long_arg_token(const char *name, const long long_val)
{
    ArgToken *a = malloc(sizeof(ArgToken));
    assert(a != NULL);
    a->name = strdup(name);
    a->token_type = ARG_TOKEN_TYPE_LONG;
    a->value.long_val = long_val;
    return a;
}

void destroy_arg_token(ArgToken *a) {
    a->name = NULL;
    a->value.string_val = NULL;
    free(a);
}

ArgTokenNode *empty_arg_token_node(void) {
    ArgTokenNode *arg_token_node = malloc(sizeof(ArgTokenNode));
    assert(arg_token_node != NULL);
    return arg_token_node;
}

ArgTokenNode *new_arg_token_node(ArgToken *a)
{
    ArgTokenNode *arg_token_node = malloc(sizeof(ArgTokenNode));
    assert(arg_token_node != NULL);
    arg_token_node->token = a;
    arg_token_node->next = NULL;
    return arg_token_node;
}

void add_arg_token_to_arg_token_node(ArgToken *a, ArgTokenNode *arg_token_node)
{
    if (arg_token_node->token == NULL) {
        arg_token_node->token = a;
    } else {
        ArgTokenNode *temp = arg_token_node;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_arg_token_node(a);
    }
}

bool is_null_or_empty_arg_token_node(const ArgTokenNode *arg_token_node) {
    if (arg_token_node == NULL || arg_token_node->token == NULL) {
        return 1;
    }
    return 0;
}

size_t arg_token_node_count(ArgTokenNode *arg_token_node) {
    size_t node_count = 0;
    ArgTokenNode *temp = arg_token_node;
    while (temp != NULL && temp->token != NULL) {
        node_count++;
        temp = temp->next;
    }
    return node_count;
}

void destroy_arg_token_node(ArgTokenNode *arg_token_node) {
    ArgTokenNode *temp = arg_token_node;
    while (temp != NULL) {
        if (temp->token != NULL) {
            destroy_arg_token(temp->token);
        }
        temp = temp->next;
    }
}
