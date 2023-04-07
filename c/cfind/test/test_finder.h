#ifndef TEST_FINDER_H
#define TEST_FINDER_H

#include "../include/config.h"
#include "../include/finder.h"

#include "test_common.h"

void test_validate_settings(void);

void test_is_matching_dir(void);

void test_is_matching_dir_in_dirpatterns(void);

void test_is_matching_dir_out_dirpatterns(void);

void test_filter_file(void);

void test_is_matching_file_in_extensions(void);

void test_is_matching_file_out_extensions(void);

void test_is_matching_file_in_filepatterns(void);

void test_is_matching_file_out_filepatterns(void);

void test_is_matching_file_in_filetypes(void);

void test_is_matching_file_out_filetypes(void);

#endif
