#!/bin/bash
################################################################################
#
# test_settings.bash
#
# Unit tests for settings
#
################################################################################

TESTDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

source "$TESTDIR/test_common.bash"

test_default_settings () {
    echo
    hdr "test_default_settings"

    # not sure if this works to reset the imported variables
    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=()
    settings_from_args $args

    assert_equals_string ARCHIVES_ONLY $ARCHIVES_ONLY false
    assert_equals_string DEBUG $DEBUG false
    assert_equals_string FOLLOW_SYMLINKS $FOLLOW_SYMLINKS false
    assert_equals_string INCLUDE_ARCHIVES $INCLUDE_ARCHIVES false
    assert_equals_string INCLUDE_HIDDEN $INCLUDE_HIDDEN false
    assert_equals_number '${#IN_ARCHIVE_EXTENSIONS[@]}' ${#IN_ARCHIVE_EXTENSIONS[@]} 0
    assert_equals_number '${#IN_ARCHIVE_FILE_PATTERNS[@]}' ${#IN_ARCHIVE_FILE_PATTERNS[@]} 0
    assert_equals_number '${#IN_DIR_PATTERNS[@]}' ${#IN_DIR_PATTERNS[@]} 0
    assert_equals_number '${#IN_EXTENSIONS[@]}' ${#IN_EXTENSIONS[@]} 0
    assert_equals_number '${#IN_FILE_PATTERNS[@]}' ${#IN_FILE_PATTERNS[@]} 0
    assert_equals_number '${#IN_FILE_TYPES[@]}' ${#IN_FILE_TYPES[@]} 0
    assert_equals_number MAX_DEPTH $MAX_DEPTH -1
    assert_equals_number MAX_LAST_MOD_EPOCH $MAX_LAST_MOD_EPOCH 0
    assert_equals_number MAX_SIZE $MAX_SIZE 0
    assert_equals_number MIN_DEPTH $MIN_DEPTH -1
    assert_equals_number MIN_LAST_MOD_EPOCH $MIN_LAST_MOD_EPOCH 0
    assert_equals_number MIN_SIZE $MIN_SIZE 0
    assert_equals_string NEED_FILE_SIZE $NEED_FILE_SIZE false
    assert_equals_string NEED_FILE_TYPE $NEED_FILE_TYPE false
    assert_equals_string NEED_LAST_MOD $NEED_LAST_MOD false
    assert_equals_number '${#OUT_ARCHIVE_EXTENSIONS[@]}' ${#OUT_ARCHIVE_EXTENSIONS[@]} 0
    assert_equals_number '${#OUT_ARCHIVE_FILE_PATTERNS[@]}' ${#OUT_ARCHIVE_FILE_PATTERNS[@]} 0
    assert_equals_number '${#OUT_DIR_PATTERNS[@]}' ${#OUT_DIR_PATTERNS[@]} 0
    assert_equals_number '${#OUT_EXTENSIONS[@]}' ${#OUT_EXTENSIONS[@]} 0
    assert_equals_number '${#OUT_FILE_PATTERNS[@]}' ${#OUT_FILE_PATTERNS[@]} 0
    assert_equals_number '${#OUT_FILE_TYPES[@]}' ${#OUT_FILE_TYPES[@]} 0
    assert_equals_number '${#PATHS[@]}' ${#PATHS[@]} 0
    assert_equals_string PRINT_DIRS $PRINT_DIRS false
    assert_equals_string PRINT_FILES $PRINT_FILES false
    assert_equals_string PRINT_USAGE $PRINT_USAGE false
    assert_equals_string PRINT_VERSION $PRINT_VERSION false
    assert_equals_string RECURSIVE $RECURSIVE true
    assert_equals_string SORT_BY $SORT_BY path
    assert_equals_string SORT_CASE_SENSITIVE $SORT_CASE_SENSITIVE false
    assert_equals_string SORT_DESCENDING $SORT_DESCENDING false
    assert_equals_string VERBOSE $VERBOSE false
}

test_settings_from_args () {
    echo
    hdr "test_settings_from_args"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"
    # DEBUG=true

    args=(-D build --debug --followsymlinks --includehidden --maxdepth 5 --maxlastmod '2024-01-01' --mindepth 2 --minlastmod '2020-01-01' --printdirs --printfiles --sort-by size --sort-casesensitive --sort-descending -t code -x c,h .)

    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    assert_equals_string DEBUG $DEBUG true
    assert_equals_string FOLLOW_SYMLINKS $FOLLOW_SYMLINKS true
    assert_equals_string INCLUDE_HIDDEN $INCLUDE_HIDDEN true
    assert_equals_number '${#IN_EXTENSIONS[@]}' ${#IN_EXTENSIONS[@]} 2
    assert_equals_string '${IN_EXTENSIONS[0]}' ${IN_EXTENSIONS[0]} c
    assert_equals_string '${IN_EXTENSIONS[1]}' ${IN_EXTENSIONS[1]} h
    assert_equals_number '${#IN_FILE_TYPES[@]}' ${#IN_FILE_TYPES[@]} 1
    assert_equals_string '${IN_FILE_TYPES[0]}' ${IN_FILE_TYPES[0]} code
    assert_equals_number MAX_DEPTH $MAX_DEPTH 5
    assert_greater_than_number MAX_LAST_MOD_EPOCH $MAX_LAST_MOD_EPOCH 0
    assert_equals_number MIN_DEPTH $MIN_DEPTH 2
    assert_greater_than_number MIN_LAST_MOD_EPOCH $MIN_LAST_MOD_EPOCH 0
    assert_equals_string NEED_FILE_SIZE $NEED_FILE_SIZE true
    assert_equals_string NEED_FILE_TYPE $NEED_FILE_TYPE true
    assert_equals_number '${#OUT_DIR_PATTERNS[@]}' ${#OUT_DIR_PATTERNS[@]} 1
    assert_equals_string '${OUT_DIR_PATTERNS[0]}' ${OUT_DIR_PATTERNS[0]} build
    assert_equals_number '${#PATHS[@]}' ${#PATHS[@]} 1
    assert_equals_string '${PATHS[0]}' ${PATHS[0]} .
    assert_equals_string PRINT_DIRS $PRINT_DIRS true
    assert_equals_string PRINT_FILES $PRINT_FILES true
    assert_equals_string SORT_BY $SORT_BY size
    assert_equals_string SORT_CASE_SENSITIVE $SORT_CASE_SENSITIVE true
    assert_equals_string SORT_DESCENDING $SORT_DESCENDING true
    # because DEBUG=true
    assert_equals_string VERBOSE $VERBOSE true
}

test_settings_from_args_invalid_option () {
    echo
    hdr "test_settings_from_args_invalid_option"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build --debug . --invalid)

    echo "args: ${args[@]}"

    result=$(settings_from_args "${args[@]}")

    if [[ $result == *"ERROR: Invalid option: --invalid"* ]]
    then
        assertion_passed "\$result contains 'ERROR: Invalid option: --invalid'"
    else
        assertion_failed "\$result does not contain 'ERROR: Invalid option: --invalid'"
    fi
}

test_settings_from_args_missing_arg_for_option () {
    echo
    hdr "test_settings_from_args_missing_arg_for_option"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build --debug . -x)

    echo "args: ${args[@]}"

    result=$(settings_from_args "${args[@]}")

    if [[ $result == *"ERROR: Missing argument for option"* ]]
    then
        assertion_passed "\$result contains 'ERROR: Missing argument for option'"
    else
        assertion_failed "\$result does not contain 'ERROR: Missing argument for option'"
    fi
}

test_validate_settings_valid_settings () {
    echo
    hdr "test_validate_settings_valid_settings"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build -t code .)

    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    result=$(validate_settings)

    if [[ $result == '' ]]
    then
        assertion_passed "\$result is empty string (no errors)"
    else
        assertion_failed "\$result is not empty string (errors)"
    fi
}

test_validate_settings_missing_path () {
    echo
    hdr "test_validate_settings_missing_path"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build -t code)

    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    result=$(validate_settings)

    if [[ $result == *'ERROR: Startpath not defined'* ]]
    then
        assertion_passed "\$result contains 'ERROR: Startpath not defined'"
    else
        assertion_failed "\$result does not contain 'ERROR: Startpath not defined'"
    fi
}

test_validate_settings_invalid_path () {
    echo
    hdr "test_validate_settings_invalid_path"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build invalid-path)

    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    result=$(validate_settings)

    if [[ $result == *'ERROR: Startpath not found'* ]]
    then
        assertion_passed "\$result contains 'ERROR: Startpath not found'"
    else
        assertion_failed "\$result does not contain 'ERROR: Startpath not found'"
    fi
}

test_validate_settings_invalid_depth_range () {
    echo
    hdr "test_validate_settings_invalid_depth_range"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build --maxdepth 2 --mindepth 3 .)

    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    result=$(validate_settings)

    if [[ $result == *'ERROR: Invalid range for mindepth and maxdepth'* ]]
    then
        assertion_passed "\$result contains 'ERROR: Invalid range for mindepth and maxdepth'"
    else
        assertion_failed "\$result does not contain 'ERROR: Invalid range for mindepth and maxdepth'"
    fi
}

test_validate_settings_invalid_lastmod_range () {
    echo
    hdr "test_validate_settings_invalid_lastmod_range"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build --maxlastmod '2020-01-01' --minlastmod '2021-01-01' .)

    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    result=$(validate_settings)

    if [[ $result == *'ERROR: Invalid range for minlastmod and maxlastmod'* ]]
    then
        assertion_passed "\$result contains 'ERROR: Invalid range for minlastmod and maxlastmod'"
    else
        assertion_failed "\$result does not contain 'ERROR: Invalid range for minlastmod and maxlastmod'"
    fi
}

test_validate_settings_invalid_size_range () {
    echo
    hdr "test_validate_settings_invalid_size_range"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    args=(-D build --maxsize 1000 --minsize 1001 .)

    echo "args: ${args[@]}"

    settings_from_args "${args[@]}"

    result=$(validate_settings)

    if [[ $result == *'ERROR: Invalid range for minsize and maxsize'* ]]
    then
        assertion_passed "\$result contains 'ERROR: Invalid range for minsize and maxsize'"
    else
        assertion_failed "\$result does not contain 'ERROR: Invalid range for minsize and maxsize'"
    fi
}

test_settings_from_json () {
    echo
    hdr "test_settings_from_json"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    json='{
        "path": "~/src/xfind/",
        "in-ext": [
            "js",
            "ts"
        ],
        "out-dirpattern": [
            "_",
            "ansible",
            "bak",
            "build",
            "chef",
            "dist",
            "node_module",
            "target",
            "test",
            "typings",
            "vendor"
        ],
        "out-filepattern": [
            "gulpfile",
            "\\.min\\."
        ],
        "debug": true,
        "followsymlinks": true,
        "includehidden": false,
        "maxdepth": 10,
        "printdirs": true,
        "printfiles": true
    }'

    settings_from_json "$json"

    assert_equals_string DEBUG $DEBUG true
    assert_equals_string FOLLOW_SYMLINKS $FOLLOW_SYMLINKS true
    assert_equals_number '${#IN_EXTENSIONS[@]}' ${#IN_EXTENSIONS[@]} 2
    assert_equals_string '${IN_EXTENSIONS[0]}' ${IN_EXTENSIONS[0]} js
    assert_equals_string '${IN_EXTENSIONS[1]}' ${IN_EXTENSIONS[1]} ts
    assert_equals_string INCLUDE_HIDDEN $INCLUDE_HIDDEN false
    assert_equals_number MAX_DEPTH $MAX_DEPTH 10
    assert_equals_number '${#OUT_DIR_PATTERNS[@]}' ${#OUT_DIR_PATTERNS[@]} 11
    assert_equals_number '${#OUT_FILE_PATTERNS[@]}' ${#OUT_FILE_PATTERNS[@]} 2
    assert_equals_number '${#PATHS[@]}' ${#PATHS[@]} 1
    assert_contains_string '${PATHS[0]}' ${PATHS[0]} src/xfind
    assert_equals_string PRINT_DIRS $PRINT_DIRS true
    assert_equals_string PRINT_FILES $PRINT_FILES true
    assert_equals_string VERBOSE $VERBOSE true
}

test_settings_from_file () {
    echo
    hdr "test_settings_from_file"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    settings_file="$SHARED_PATH/settings.json"

    settings_from_file "$settings_file"

    assert_equals_string DEBUG $DEBUG true
    assert_equals_string FOLLOW_SYMLINKS $FOLLOW_SYMLINKS true
    assert_equals_number '${#IN_EXTENSIONS[@]}' ${#IN_EXTENSIONS[@]} 2
    assert_equals_string '${IN_EXTENSIONS[0]}' ${IN_EXTENSIONS[0]} js
    assert_equals_string '${IN_EXTENSIONS[1]}' ${IN_EXTENSIONS[1]} ts
    assert_equals_string INCLUDE_HIDDEN $INCLUDE_HIDDEN false
    assert_equals_number MAX_DEPTH $MAX_DEPTH 10
    assert_equals_number '${#OUT_DIR_PATTERNS[@]}' ${#OUT_DIR_PATTERNS[@]} 11
    assert_equals_number '${#OUT_FILE_PATTERNS[@]}' ${#OUT_FILE_PATTERNS[@]} 2
    assert_equals_number '${#PATHS[@]}' ${#PATHS[@]} 1
    assert_contains_string '${PATHS[0]}' ${PATHS[0]} src/xfind
    assert_equals_string PRINT_DIRS $PRINT_DIRS true
    assert_equals_string PRINT_FILES $PRINT_FILES true
    assert_equals_string VERBOSE $VERBOSE true
}
