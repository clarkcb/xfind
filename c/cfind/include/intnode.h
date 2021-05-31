#ifndef INTNODE_H
#define INTNODE_H

typedef struct IntNode {
    const int *integer;
    struct IntNode *next;
} IntNode;

IntNode *empty_int_node(void);

IntNode *new_int_node(const int *i);

void add_int_to_int_node(const int *i, IntNode *int_node);

int is_null_or_empty_int_node(IntNode *int_node);

int int_matches_int_node(const int *i, IntNode *int_node);

size_t int_node_count(IntNode *int_node);

void destroy_int_node(IntNode *int_node);

#endif
