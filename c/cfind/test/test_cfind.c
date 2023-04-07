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
    test_filetype_from_name();
    test_get_filetype_for_ext();
    test_is_filetype_for_ext();

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

	// test_findsettings
	test_default_settings();

	// test_finder
	test_validate_settings();
	test_is_matching_dir();
    test_is_matching_dir_in_dirpatterns();
    test_is_matching_dir_out_dirpatterns();
    test_filter_file();
    test_is_matching_file_in_extensions();
    test_is_matching_file_out_extensions();
    test_is_matching_file_in_filepatterns();
    test_is_matching_file_out_filepatterns();
    test_is_matching_file_in_filetypes();
    test_is_matching_file_out_filetypes();

    return 0;
}
