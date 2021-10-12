#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "intnode.h"

IntNode *empty_int_node(void)
{
    IntNode *int_node = malloc(sizeof(IntNode));
    assert(int_node != NULL);
    int_node->integer = NULL;
    int_node->next = NULL;
    return int_node;
}

IntNode *new_int_node(const int *i)
{
    IntNode *int_node = malloc(sizeof(IntNode));
    assert(int_node != NULL);
    int_node->integer = i;
    int_node->next = NULL;
    return int_node;
}

void add_int_to_int_node(const int *i, IntNode *int_node)
{
    if (int_node->integer == NULL) {
        int_node->integer = i;
    } else {
        IntNode *temp = int_node;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_int_node(i);
    }
}

int is_null_or_empty_int_node(IntNode *int_node)
{
    if (int_node == NULL || (int_node->integer == NULL && int_node->next == NULL))
        return 1;
    return 0;
}

int int_matches_int_node(const int *i, IntNode *int_node)
{
    IntNode *temp = int_node;
    int matches = 0;
    while (matches == 0 && temp != NULL) {
        if (*(temp->integer) == *i) {
            matches++;
        }
        temp = temp->next;
    }
    return matches;
}

size_t int_node_count(IntNode *int_node)
{
    size_t nodecount = 0;
    IntNode *temp = int_node;
    while (temp != NULL) {
        nodecount++;
        temp = temp->next;
    }
    return nodecount;
}

void destroy_int_node(IntNode *int_node)
{
    IntNode *current = int_node;
    IntNode *next;
    while (current != NULL) {
        next = current->next;
        free(current);
        current = next;
    }
}
