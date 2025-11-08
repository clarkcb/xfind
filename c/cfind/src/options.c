#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <cjson/cJSON.h>

#include "filetypes.h"
#include "options.h"

Option *new_option(const char *long_arg, const char *short_arg, const char *desc, const int arg_type)
{
    Option *o = malloc(sizeof(Option));
    assert(o != NULL);
    o->long_arg = long_arg;
    o->short_arg = short_arg;
    o->description = desc;
    o->arg_type = arg_type;
    return o;
}

Options *empty_options(void)
{
    Options *options = malloc(sizeof(Options));
    assert(options != NULL);
    options->option = NULL;
    options->next = NULL;
    return options;
}

Options *new_options(Option *o)
{
    Options *options = malloc(sizeof(Options));
    assert(options != NULL);
    options->option = o;
    options->next = NULL;
    return options;
}

void add_to_options(Option *o, Options *options)
{
    assert(options != NULL);
    if (options->option == NULL) {
        options->option = o;
    } else {
        Options *temp = options;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = new_options(o);
    }
}

Option *find_option_for_long_arg(const char *arg_name, Options *options)
{
    assert(options != NULL);
    Options *temp = options;
    Option *o = options->option;
    while (temp != NULL && o != NULL) {
        if (strcmp(arg_name, o->long_arg) == 0) return o;
        temp = temp->next;
        o = temp == NULL ? NULL : temp->option;
    }
    return o;
}

Option *find_option_for_short_arg(const char *arg_name, Options *options)
{
    assert(options != NULL);
    Options *temp = options;
    Option *o = options->option;
    while (temp != NULL && o != NULL) {
        if (o->short_arg != NULL && strcmp(arg_name, o->short_arg) == 0) return o;
        temp = temp->next;
        o = temp == NULL ? NULL : temp->option;
    }
    return o;
}

size_t options_count(Options *options)
{
    Options *temp = options;
    size_t optcount = 0;
    while (temp != NULL && temp->option != NULL) {
        optcount++;
        temp = temp->next;
    }
    return optcount;
}

void destroy_option(Option *o)
{
    if (o->short_arg != NULL) {
        free(o->short_arg);
    }
    free(o->long_arg);
    free(o->description);
    free(o);
}

void destroy_options(Options *options)
{
    if (options != NULL) {
        Options *current = options;
        while (current != NULL) {
            destroy_option(current->option);
            Options *next = current->next;
            free(current);
            current = next;
        }
    }
}
