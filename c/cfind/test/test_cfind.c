#include "test_common.h"
#include "test_filetypes.h"
#include "test_fileutil.h"
#include "test_finder.h"
#include "test_findoptions.h"
#include "test_findsettings.h"

int main(int argc, char *argv[])
{
	// test_common
	test_last_index_of_char_in_string();

	// test_filetypes
    test_file_type_from_name();
    test_get_file_type_for_ext();
    test_is_file_type_for_ext();

	// test_fileutil
    test_dir_or_file_exists();
    test_get_extension();
	test_is_dot_dir();
	test_is_hidden();
    test_expand_path();
    test_join_path();
    test_split_path();

	// test_findoptions
	test_settings_from_args();
	test_settings_from_json_string();
	// test_settings_from_json_file();

	// test_findsettings
	test_default_settings();
    test_add_extensions_to_settings();
    test_add_patterns_to_settings();
    test_set_archives_only_in_settings();
    test_set_debug_in_settings();

    // test_finder
	test_validate_settings();
	test_is_matching_dir();
    test_is_matching_dir_in_dir_patterns();
    test_is_matching_dir_out_dir_patterns();
    test_filter_path();
    test_is_matching_path_in_extensions();
    test_is_matching_path_out_extensions();
    test_is_matching_path_in_file_patterns();
    test_is_matching_path_out_file_patterns();
    test_is_matching_path_in_file_types();
    test_is_matching_path_out_file_types();

    return 0;
}
