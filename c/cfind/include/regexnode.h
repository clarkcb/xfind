#ifndef REGEXNODE_H
#define REGEXNODE_H

#include "regex.h"

typedef struct Regex {
    const char *pattern;
    regex_t compiled;
} Regex;

typedef struct RegexNode {
    Regex *regex;
    struct RegexNode *next;
} RegexNode;

Regex *new_regex(const char *pat);

RegexNode *new_regex_node(Regex *r);

RegexNode *new_regex_node_from_string(const char *pat);

void add_regex_to_regex_node(Regex *r, RegexNode *regex_node);

void add_string_to_regex_node(const char *pat, RegexNode *regex_node);

int is_null_or_empty_regex_node(const RegexNode *regex_node);

int string_matches_regex_node(const char *s, RegexNode *regex_node);

size_t regex_node_count(RegexNode *regex_node);

size_t regex_node_strlen(RegexNode *regex_node);

void regex_node_to_string(RegexNode *regex_node, char *s);

void destroy_regex(Regex *r);

void destroy_regex_node(RegexNode *regex_node);

#endif
