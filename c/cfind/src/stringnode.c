#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "intnode.h"
#include "stringnode.h"

StringNode *empty_string_node(void)
{
    StringNode *string_node = malloc(sizeof(StringNode));
    assert(string_node != NULL);
    string_node->string = NULL;
    string_node->next = NULL;
    return string_node;
}

StringNode *new_string_node(const char *s)
{
    StringNode *string_node = malloc(sizeof(StringNode));
    assert(string_node != NULL);
    string_node->string = s;
    string_node->next = NULL;
    return string_node;
}

StringNode *new_string_node_from_char_split(const char c, const char *s)
{
    StringNode *string_node = empty_string_node();
    assert(string_node != NULL);
    add_char_split_to_string_node(c, s, string_node);
    return string_node;
}

void add_string_to_string_node(const char *s, StringNode *string_node)
{
    if (string_node->string == NULL) {
        string_node->string = s;
    } else {
        StringNode *temp = string_node;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_string_node(s);
    }
}

void add_char_split_to_string_node(const char c, const char *s, StringNode *string_node)
{
    if (s == NULL) return;
    size_t slen = strlen(s);
    if (slen == 0) return;
    if (char_in_string(c, s) == 0) {
        add_string_to_string_node(s, string_node);
        return;
    }

    unsigned int startidx = 0;
    // skip leading split char
    while (s[startidx] == c) {
        startidx++;
    }
    unsigned int endidx = (unsigned int)slen - 1;
    // skip trailing split char
    while (s[endidx] == c) {
        endidx--;
    }
    assert(endidx > startidx);

    IntNode *int_node = empty_int_node();
    for (unsigned int i=startidx; i <= endidx; i++) {
        if (s[i] == c) {
            int *j = malloc(sizeof(int));
            *j = (int)i;
            add_int_to_int_node(j, int_node);
        }
    }

    IntNode *temp = int_node;
    while (temp != NULL && temp->integer != NULL) {
        unsigned int i = (unsigned int)*(temp->integer);
        if (i > startidx) {
            char *ns = malloc((unsigned long)(i - startidx + 1) * sizeof(char));
            strncpy(ns, s + startidx, i - startidx);
            ns[i - startidx] = '\0';
            add_string_to_string_node(ns, string_node);
            startidx = i + 1;
        }
        temp = temp->next;
    }
    if (endidx >= startidx) {
        unsigned int nslen = endidx - startidx + 1;
        char *ns = malloc((unsigned long)(nslen + 1) * sizeof(char));
        strncpy(ns, s + startidx, nslen);
        ns[nslen] = '\0';
        add_string_to_string_node(ns, string_node);
    }
}

int is_null_or_empty_string_node(const StringNode *string_node)
{
    if (string_node == NULL || (string_node->string == NULL && string_node->next == NULL))
        return 1;
    return 0;
}

int string_matches_string_node(const char *s, StringNode *string_node)
{
    if (s == NULL || string_node == NULL) return 0;
    StringNode *temp = string_node;
    int matches = 0;
    while (matches == 0 && temp != NULL && temp->string != NULL) {
        // TODO: right now this matches strings, need to switch to regex
        if (strncmp(s, temp->string, 255) == 0) {
            matches++;
        }
        temp = temp->next;
    }
    return matches;
}

size_t string_node_count(StringNode *string_node)
{
    size_t nodecount = 0;
    StringNode *temp = string_node;
    while (temp != NULL && temp->string != NULL) {
        nodecount++;
        temp = temp->next;
    }
    return nodecount;
}

size_t string_node_strlen(StringNode *string_node)
{
    size_t slen = 2; // for '[' and ']'
    StringNode *temp = string_node;
    unsigned int nodecount = 0;
    while (temp != NULL && temp->string != NULL) {
        slen += strlen(temp->string) + 2; // for ""
        temp = temp->next;
        nodecount++;
    }
    if (nodecount > 1) {
        slen += (nodecount - 1); // for commas
    }
    return slen;
}

void string_node_to_string(StringNode *string_node, char *s)
{
    // assumes s has correct allocation size
    // int i = 0;
    strcat(s, "[");

    StringNode *temp = string_node;
    int strcount = 0;

    while (temp != NULL) {
        if (strcount > 0) {
            strcat(s, ", ");
        }
        strcat(s, "\"");
        strcat(s, temp->string);
        // i += strlen(temp->string);
        strcat(s, "\"");
        temp = temp->next;
        strcount++;
    }

    strcat(s, "]");
}

void destroy_string_node(StringNode *string_node)
{
    StringNode *current = string_node;
    StringNode *next;
    while (current != NULL) {
        current->string = NULL;
        next = current->next;
        free(current);
        current = next;
    }
    // free(string_node);
}
