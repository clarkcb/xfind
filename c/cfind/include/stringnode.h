#ifndef STRINGNODE_H
#define STRINGNODE_H

#include <stdbool.h>

typedef struct StringNode {
    const char *string;
    struct StringNode *next;
} StringNode;

StringNode *empty_string_node(void);

StringNode *new_string_node(const char *s);

StringNode *new_string_node_from_char_split(char c, const char *s);

void add_string_to_string_node(const char *s, StringNode *string_node);

void add_char_split_to_string_node(char c, const char *s, StringNode *string_node);

bool is_null_or_empty_string_node(const StringNode *string_node);

bool string_matches_string_node(const char *s, const StringNode *string_node);

size_t string_node_count(const StringNode *string_node);

size_t string_node_strlen(const StringNode *string_node);

void string_node_to_string(const StringNode *string_node, char *s);

void destroy_string_node(StringNode *string_node);

#endif
