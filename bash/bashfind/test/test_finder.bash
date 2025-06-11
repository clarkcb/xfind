#!/bin/bash
################################################################################
#
# test_finder.bash
#
# Unit tests for finder functions
#
################################################################################

TESTDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

source "$TESTDIR/test_common.bash"

#------------------------------------------------
# is_matching_dir
#------------------------------------------------
test_is_matching_dir_default_settings () {
    echo
    hdr "test_is_matching_dir_default_settings"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    dir1="."
    is_matching_dir "$dir1"
    assert_equals_number "is_matching_dir $dir1" $? 1

    dir2=".."
    is_matching_dir "$dir2"
    assert_equals_number "is_matching_dir $dir2" $? 1

    dir3=".git"
    is_matching_dir "$dir3"
    assert_equals_number "is_matching_dir $dir3" $? 0
}

test_is_matching_dir_in_dir_patterns () {
    echo
    hdr "test_is_matching_dir_in_dir_patterns"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-d bash)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    dir1="."
    is_matching_dir "$dir1"
    assert_equals_number "is_matching_dir $dir1" $? 1

    dir2=".."
    is_matching_dir "$dir2"
    assert_equals_number "is_matching_dir $dir2" $? 1

    dir3="$BASHFIND_PATH"
    is_matching_dir "$dir3"
    assert_equals_number "is_matching_dir $dir3" $? 1

    dir4="$XFIND_PATH"
    is_matching_dir "$dir4"
    assert_equals_number "is_matching_dir $dir4" $? 0
}

test_is_matching_dir_out_dir_patterns () {
    echo
    hdr "test_is_matching_dir_out_dir_patterns"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D bash)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    dir1="."
    is_matching_dir "$dir1"
    assert_equals_number "is_matching_dir $dir1" $? 1

    dir2=".."
    is_matching_dir "$dir2"
    assert_equals_number "is_matching_dir $dir2" $? 1

    dir3="$BASHFIND_PATH"
    is_matching_dir "$dir3"
    assert_equals_number "is_matching_dir $dir3" $? 0

    dir4="$XFIND_PATH"
    is_matching_dir "$dir4"
    assert_equals_number "is_matching_dir $dir4" $? 1
}


#------------------------------------------------
# is_matching_archive_file
#------------------------------------------------
test_is_matching_archive_file_default_settings () {
    echo
    hdr "test_is_matching_archive_file_default_settings"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    file1="./archive.zip"
    is_matching_archive_file "$file1"
    assert_equals_number "is_matching_archive_file $file1" $? 1

    file2="./javafind.jar"
    is_matching_archive_file "$file2"
    assert_equals_number "is_matching_archive_file $file2" $? 1

    file3=".hidden.zip"
    is_matching_archive_file "$file3"
    assert_equals_number "is_matching_archive_file $file3" $? 0
}

test_is_matching_archive_file_in_archive_extensions () {
    echo
    hdr "test_is_matching_archive_file_in_archive_extensions"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(--in-archiveext zip)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="./archive.zip"
    is_matching_archive_file "$file1"
    assert_equals_number "is_matching_archive_file $file1" $? 1

    file2="./javafind.jar"
    is_matching_archive_file "$file2"
    assert_equals_number "is_matching_archive_file $file2" $? 0

    file3=".hidden.zip"
    is_matching_archive_file "$file3"
    assert_equals_number "is_matching_archive_file $file3" $? 0
}

test_is_matching_archive_file_out_archive_extensions () {
    echo
    hdr "test_is_matching_archive_file_out_archive_extensions"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(--out-archiveext zip)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="./archive.zip"
    is_matching_archive_file "$file1"
    assert_equals_number "is_matching_archive_file $file1" $? 0

    file2="./javafind.jar"
    is_matching_archive_file "$file2"
    assert_equals_number "is_matching_archive_file $file2" $? 1

    file3="./arch.tar.gz"
    is_matching_archive_file "$file3"
    assert_equals_number "is_matching_archive_file $file3" $? 1
}

test_is_matching_archive_file_in_archive_file_patterns () {
    echo
    hdr "test_is_matching_archive_file_in_archive_file_patterns"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(--in-archivefilepattern archive)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="./archive.zip"
    is_matching_archive_file "$file1"
    assert_equals_number "is_matching_archive_file $file1" $? 1

    file2="./javafind.jar"
    is_matching_archive_file "$file2"
    assert_equals_number "is_matching_archive_file $file2" $? 0

    file3="./arch.tar.gz"
    is_matching_archive_file "$file3"
    assert_equals_number "is_matching_archive_file $file3" $? 0
}

test_is_matching_archive_file_out_archive_file_patterns () {
    echo
    hdr "test_is_matching_archive_file_out_archive_file_patterns"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(--out-archivefilepattern archive)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="./archive.zip"
    is_matching_archive_file "$file1"
    assert_equals_number "is_matching_archive_file $file1" $? 0

    file2="./javafind.jar"
    is_matching_archive_file "$file2"
    assert_equals_number "is_matching_archive_file $file2" $? 1

    file3="./arch.tar.gz"
    is_matching_archive_file "$file3"
    assert_equals_number "is_matching_archive_file $file3" $? 1
}


#------------------------------------------------
# is_matching_file
#------------------------------------------------
test_is_matching_file_default_settings () {
    echo
    hdr "test_is_matching_file_default_settings"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" unknown 0 0
    assert_equals_number "is_matching_file $file1" $? 1

    file2="./test_finder.bash"
    is_matching_file "$file2" unknown 0 0
    assert_equals_number "is_matching_file $file2" $? 1

    file3=".gitignore"
    is_matching_file "$file3" unknown 0 0
    assert_equals_number "is_matching_file $file3" $? 0
}

test_is_matching_file_in_extensions () {
    echo
    hdr "test_is_matching_file_in_extensions"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-x bash)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" unknown 0 0
    assert_equals_number "is_matching_file $file1" $? 1

    file2="./test_finder.bash"
    is_matching_file "$file2" unknown 0 0
    assert_equals_number "is_matching_file $file2" $? 1

    file3="$BASHFIND_PATH/lib/color.sh"
    is_matching_file "$file3" unknown 0 0
    assert_equals_number "is_matching_dir $file3" $? 0
}

test_is_matching_file_out_extensions () {
    echo
    hdr "test_is_matching_file_out_extensions"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-X bash)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" unknown 0 0
    assert_equals_number "is_matching_file $file1" $? 0

    file2="./test_finder.bash"
    is_matching_file "$file2" unknown 0 0
    assert_equals_number "is_matching_file $file2" $? 0

    file3="$BASHFIND_PATH/lib/color.sh"
    is_matching_file "$file3" unknown 0 0
    assert_equals_number "is_matching_dir $file3" $? 1
}

test_is_matching_file_in_file_patterns () {
    echo
    hdr "test_is_matching_file_in_file_patterns"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-f bash)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" unknown 0 0
    assert_equals_number "is_matching_file $file1" $? 1

    file2="./test_finder.bash"
    is_matching_file "$file2" unknown 0 0
    assert_equals_number "is_matching_file $file2" $? 0

    file3="$BASHFIND_PATH/lib/color.sh"
    is_matching_file "$file3" unknown 0 0
    assert_equals_number "is_matching_file $file3" $? 0
}

test_is_matching_file_out_file_patterns () {
    echo
    hdr "test_is_matching_file_out_file_patterns"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-F bash)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" unknown 0 0
    assert_equals_number "is_matching_file $file1" $? 0

    file2="./test_finder.bash"
    is_matching_file "$file2" unknown 0 0
    assert_equals_number "is_matching_file $file2" $? 1

    file3="$BASHFIND_PATH/lib/color.sh"
    is_matching_file "$file3" unknown 0 0
    assert_equals_number "is_matching_file $file3" $? 1
}

test_is_matching_file_in_file_types () {
    echo
    hdr "test_is_matching_file_in_file_types"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-t code)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" code 0 0
    assert_equals_number "is_matching_file $file1" $? 1

    file2="./test_finder.bash"
    is_matching_file "$file2" code 0 0
    assert_equals_number "is_matching_file $file2" $? 1

    file3="$BASHFIND_PATH/lib/color.sh"
    is_matching_file "$file3" code 0 0
    assert_equals_number "is_matching_file $file3" $? 1

    file4="./requirements.txt"
    is_matching_file "$file4" text 0 0
    assert_equals_number "is_matching_file $file4" $? 0
}

test_is_matching_file_out_file_types () {
    echo
    hdr "test_is_matching_file_out_file_types"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-T code)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" code 0 0
    assert_equals_number "is_matching_file $file1" $? 0

    file2="./test_finder.bash"
    is_matching_file "$file2" code 0 0
    assert_equals_number "is_matching_file $file2" $? 0

    file3="$BASHFIND_PATH/lib/color.sh"
    is_matching_file "$file3" code 0 0
    assert_equals_number "is_matching_file $file3" $? 0

    file4="./requirements.txt"
    is_matching_file "$file4" text 0 0
    assert_equals_number "is_matching_file $file4" $? 1
}

test_is_matching_file_by_size () {
    echo
    hdr "test_is_matching_file_by_size"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(--minsize 1000 --maxsize 10000)
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    is_matching_file "$file1" code 1100 0
    assert_equals_number "is_matching_file $file1 1000" $? 1

    file2="./test_finder.bash"
    is_matching_file "$file2" code 9000 0
    assert_equals_number "is_matching_file $file2 9000" $? 1

    file3="$BASHFIND_PATH/lib/color.sh"
    is_matching_file "$file3" code 900 0
    assert_equals_number "is_matching_file $file3 900" $? 0

    file4="./requirements.txt"
    is_matching_file "$file4" text 10001 0
    assert_equals_number "is_matching_file $file4 10001" $? 0
}


#------------------------------------------------
# follow_symlinks
#------------------------------------------------

test_follow_symlinks_default () {
    echo
    hdr "test_follow_symlinks_default"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=("$XFIND_PATH/bin")
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    do_find
    if [ ${#FILE_RESULTS[@]} -eq 0 ]
    then
        assert_equals_number "follow_symlinks_default" 0 0
    else
        assert_less_than_number "follow_symlinks_default" ${#FILE_RESULTS[@]} 4
    fi
}

test_follow_symlinks () {
    echo
    hdr "test_follow_symlinks"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(--followsymlinks "$XFIND_PATH/bin")
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    do_find
    if [ ${#FILE_RESULTS[@]} -eq 0 ]
    then
        assert_equals_number "test_follow_symlinks" 0 0
    else
        assert_greater_than_number "test_follow_symlinks" ${#FILE_RESULTS[@]} 2
    fi
}

test_no_follow_symlinks () {
    echo
    hdr "test_no_follow_symlinks"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(--nofollowsymlinks "$XFIND_PATH/bin")
    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    file1="$BASHFIND_PATH/lib/bashfindlib.bash"
    do_find
    if [ ${#FILE_RESULTS[@]} -eq 0 ]
    then
        assert_equals_number "test_no_follow_symlinks" 0 0
    else
        assert_less_than_number "test_no_follow_symlinks" ${#FILE_RESULTS[@]} 4
    fi
}
