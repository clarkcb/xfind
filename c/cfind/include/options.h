#ifndef OPTIONS_H
#define OPTIONS_H

typedef struct Option {
    const char *long_arg;
    const char *short_arg;
    const char *description;
    int arg_type;
} Option;

typedef struct Options {
    Option *option;
    struct Options *next;
} Options;

Option *new_option(const char *long_arg, const char *short_arg, const char *desc, int arg_type);

Options *empty_options(void);

Options *new_options(Option *o);

void add_to_options(Option *o, Options *options);

Option *find_option_for_long_arg(const char *arg_name, Options *options);

Option *find_option_for_short_arg(const char *arg_name, Options *options);

size_t options_count(Options *options);

void destroy_option(Option *o);

void destroy_options(Options *options);

#endif
