#!/bin/bash
################################################################################
#
# bashfindtests.bash
#
# Unit tests for bash version of xfind
#
# Requirements:
# - bash
# - find
# - grep
# - sed
# - sort
# - jq
#
################################################################################

TESTDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Settings tests
source "$TESTDIR/test_settings.bash"

test_default_settings

test_settings_from_args

test_settings_from_args_invalid_option

test_settings_from_args_missing_arg_for_option

test_validate_settings_valid_settings

test_validate_settings_missing_path

test_validate_settings_invalid_path

test_validate_settings_invalid_depth_range

test_validate_settings_invalid_lastmod_range

test_validate_settings_invalid_size_range

test_settings_from_json

test_settings_from_file


# # File types tests
source "$TESTDIR/test_file_types.bash"

test_is_file_type

test_get_file_type


# Finder tests
source "$TESTDIR/test_finder.bash"

test_is_matching_dir_default_settings

test_is_matching_dir_in_dir_patterns

test_is_matching_dir_out_dir_patterns

test_is_matching_archive_file_default_settings

test_is_matching_archive_file_in_archive_extensions

test_is_matching_archive_file_out_archive_extensions

test_is_matching_archive_file_in_archive_file_patterns

test_is_matching_archive_file_out_archive_file_patterns

test_is_matching_file_default_settings

test_is_matching_file_in_extensions

test_is_matching_file_out_extensions

test_is_matching_file_in_file_patterns

test_is_matching_file_out_file_patterns

test_is_matching_file_in_file_types

test_is_matching_file_out_file_types

test_is_matching_file_by_size

