#include "test_common.h"
#include "test_filetypes.h"
#include "test_fileutil.h"
#include "test_findoptions.h"
#include "test_findsettings.h"

int main(int argc, char *argv[])
{
	// test_common
	test_last_index_of_char_in_string();

	// test_filetypes
    test_filetype_from_name();

	// test_fileutil
    test_dir_or_file_exists();

	test_is_hidden();

	// test_findoptions
	test_settings_from_args();

	// test_findsettings
	test_default_settings();

    return 0;
}
