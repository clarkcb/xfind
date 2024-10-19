#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "regexnode.h"

#include <stdio.h>

Regex *new_regex(const char *pat)
{
    Regex *regex = malloc(sizeof(Regex));
    assert(regex != NULL);
    regex->pattern = pat;
    const int res = regcomp(&regex->compiled, pat, 0);
    if (res > 1) printf("An error occurred trying to compile regex pattern \"%s\": %d", pat, res);
    assert(res == 0);
    return regex;
}

size_t regex_strlen(Regex *regex)
{
    return strnlen(regex->pattern, 1024) + 2; // for ""
}

RegexNode *new_regex_node(Regex *r)
{
    RegexNode *regex_node = malloc(sizeof(RegexNode));
    assert(regex_node != NULL);
    regex_node->regex = r;
    regex_node->next = NULL;
    return regex_node;
}

RegexNode *new_regex_node_from_string(const char *pat)
{
    Regex *regex = new_regex(pat);
    return new_regex_node(regex);
}

void add_regex_to_regex_node(Regex *r, RegexNode *regex_node)
{
    if (regex_node->regex == NULL) {
        regex_node->regex = r;
    } else {
        RegexNode *temp = regex_node;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_regex_node(r);
    }
}

void add_string_to_regex_node(const char *pat, RegexNode *regex_node)
{
    Regex *regex = new_regex(pat);
    add_regex_to_regex_node(regex, regex_node);
}

int is_null_or_empty_regex_node(const RegexNode *regex_node)
{
    if (regex_node == NULL || regex_node->regex == NULL)
        return 1;
    return 0;
}

int string_matches_regex_node(const char *s, RegexNode *regex_node)
{
    RegexNode *temp = regex_node;
    int matches = 0;
    while (matches == 0 && temp != NULL) {
        const int res = regexec(&temp->regex->compiled, s, 0, NULL, 0);
        if (res == 0) {
            matches++;
        } else if (res != REG_NOMATCH) {
            printf("An error occurred trying to match \"%s\": %d\n", s, res);
        }
        temp = temp->next;
    }
    return matches;
}

size_t regex_node_count(RegexNode *regex_node)
{
    size_t count = 0;
    RegexNode *temp = regex_node;
    while (temp != NULL) {
        count++;
        temp = temp->next;
    }
    return count;
}

size_t regex_node_strlen(RegexNode *regex_node)
{
    size_t slen = 2; // for '[' and ']' but not '\0'
    RegexNode *temp = regex_node;
    unsigned int nodecount = 0;
    while (temp != NULL) {
        slen += regex_strlen(temp->regex) + 2;
        temp = temp->next;
        nodecount++;
    }
    if (nodecount > 1) {
        slen += (nodecount - 1); // for commas
    }
    return slen;
}

void regex_node_to_string(RegexNode *regex_node, char *s)
{
    // assumes s has correct allocation size and correct position
    strcat(s, "[");

    RegexNode *temp = regex_node;
    int strcount = 0;

    while (temp != NULL) {
        if (strcount > 0) {
            strcat(s, ", ");
        }
        strcat(s, "\"");
        strcat(s, temp->regex->pattern);
        strcat(s, "\"");
        temp = temp->next;
        strcount++;
    }

    strcat(s, "]");
}

void destroy_regex(Regex *r)
{
    r->pattern = NULL;
    // r->compiled = {};
}

void destroy_regex_node(RegexNode *regex_node)
{
    RegexNode *current = regex_node;
    RegexNode *next;
    while (current != NULL) {
        next = current->next;
        destroy_regex(current->regex);
        free(current);
        current = next;
    }
}
