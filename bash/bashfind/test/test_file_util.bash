#!/bin/bash
################################################################################
#
# test_file_util.bash
#
# Unit tests for file_util functions
#
################################################################################

TESTDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

source "$TESTDIR/test_common.bash"

test_is_dot_dir () {
    echo
    hdr "test_is_dot_dir"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    dot_dir="."
    is_dot_dir "$dot_dir"
    assert_equals_number "is_dot_dir $dot_dir" $? 1

    dot_dot_dir=".."
    is_dot_dir "$dot_dot_dir"
    assert_equals_number "is_dot_dir $dot_dot_dir" $? 1

    dot_dash_dir="./"
    is_dot_dir "$dot_dash_dir"
    assert_equals_number "is_dot_dir $dot_dash_dir" $? 1

    dot_dot_dash_dir="../"
    is_dot_dir "$dot_dot_dash_dir"
    assert_equals_number "is_dot_dir $dot_dot_dash_dir" $? 1

    dot_a_dir=".a"
    is_dot_dir "$dot_a_dir"
    assert_equals_number "is_dot_dir $dot_a_dir" $? 0

    dot_git_dir=".git"
    is_dot_dir "$dot_git_dir"
    assert_equals_number "is_dot_dir $dot_git_dir" $? 0
}

test_expanded_path () {
    echo
    hdr "test_expanded_path"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    tilde="~"
    expanded_path=$(eval echo "$tilde")
    echo "expanded_path: $expanded_path"
    assert_equals_string expanded_path "$expanded_path" "$HOME"

    tilde_path="~/src/xfind"
    expanded_path=$(eval echo "$tilde_path")
    echo "expanded_path: $expanded_path"
    assert_equals_string expanded_path "$expanded_path" "$HOME/src/xfind"

    tilde_name_path="~cary/src/xfind"
    expanded_path=$(eval echo "$tilde_name_path")
    echo "expanded_path: $expanded_path"
    assert_equals_string expanded_path "$expanded_path" "$HOME/src/xfind"
}

test_is_hidden_path () {
    echo
    hdr "test_is_hidden_path"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    dot_dir="."
    is_hidden_path "$dot_dir"
    assert_equals_number "is_hidden_path $dot_dir" $? 0

    dot_dot_dir=".."
    is_hidden_path "$dot_dot_dir"
    assert_equals_number "is_hidden_path $dot_dot_dir" $? 0

    dot_dash_dir="./"
    is_hidden_path "$dot_dash_dir"
    assert_equals_number "is_hidden_path $dot_dash_dir" $? 0

    dot_dot_dash_dir="../"
    is_hidden_path "$dot_dot_dash_dir"
    assert_equals_number "is_hidden_path $dot_dot_dash_dir" $? 0

    dot_git_dir=".git"
    is_hidden_path "$dot_git_dir"
    assert_equals_number "is_hidden_path $dot_git_dir" $? 1

    dot_git_dash_dir=".git/"
    is_hidden_path "$dot_git_dash_dir"
    assert_equals_number "is_hidden_path $dot_git_dash_dir" $? 1

    dot_gitignore=".gitignore"
    is_hidden_path "$dot_gitignore"
    assert_equals_number "is_hidden_path $dot_gitignore" $? 1

    path_dot_gitignore="/path/to/.gitignore"
    is_hidden_path "$path_dot_gitignore"
    assert_equals_number "is_hidden_path $path_dot_gitignore" $? 1

    path_dot_gitignore="/path/to/.git/config"
    is_hidden_path "$path_dot_gitignore"
    assert_equals_number "is_hidden_path $path_dot_gitignore" $? 1


    current_path_file="./bashfindtests.bash"
    is_hidden_path "$current_path_file"
    assert_equals_number "is_hidden_path $current_path_file" $? 0

    parent_path_file="../bashfindtests.bash"
    is_hidden_path "$parent_path_file"
    assert_equals_number "is_hidden_path $parent_path_file" $? 0
}
