#ifndef TEST_FINDER_H
#define TEST_FINDER_H

#include "../include/config.h"
#include "../include/finder.h"

#include "test_common.h"

void test_validate_settings(void);

void test_is_matching_dir(void);

void test_is_matching_dir_in_dir_patterns(void);

void test_is_matching_dir_out_dir_patterns(void);

void test_filter_path(void);

void test_is_matching_path_in_extensions(void);

void test_is_matching_path_out_extensions(void);

void test_is_matching_path_in_file_patterns(void);

void test_is_matching_path_out_file_patterns(void);

void test_is_matching_path_in_file_types(void);

void test_is_matching_path_out_file_types(void);

void test_follow_symlinks_default_settings(void);

void test_follow_symlinks_follow_symlinks(void);

void test_follow_symlinks_no_follow_symlinks(void);

#endif
