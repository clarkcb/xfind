#!/bin/bash
################################################################################
#
# test_common.bash
#
# Common unit test data and functions for bashfind
#
################################################################################

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

BASHFIND_PATH="$XFIND_PATH/bash/bashfind"
SCRIPTS_PATH="$XFIND_PATH/scripts"
SHARED_PATH="$XFIND_PATH/shared"
FILE_TYPES_PATH="$XFIND_PATH/shared/filetypes.json"
FIND_OPTIONS_PATH="$XFIND_PATH/shared/findoptions.json"

source "$SCRIPTS_PATH/common.sh"

assertion_passed () {
    local msg="$1"
    echo -e "${GREEN}Assertion passed: $msg${COLOR_RESET}"
}

assertion_failed () {
    local msg="$1"
    echo -e "${RED}Assertion failed: $msg${COLOR_RESET}"
}

assert_equals_number () {
    local name="$1"
    local val=$2
    local expected=$3
    if [ $val == $expected ]
    then
        assertion_passed "$name == $expected"
    else
        assertion_failed "$name ($val) != $expected"
    fi
}

assert_greater_than_number () {
    local name="$1"
    local val=$2
    local expected=$3
    if [ $val -gt $expected ]
    then
        assertion_passed "$name -gt $expected"
    else
        assertion_failed "! $name ($val) -gt $expected"
    fi
}

assert_equals_string () {
    local name="$1"
    local val="$2"
    local expected="$3"

    if [ "$val" == "$expected" ]
    then
        assertion_passed "$name == $expected"
    else
        assertion_failed "$name ($val) != $expected"
    fi
}

assert_contains_string () {
    local name="$1"
    local string="$2"
    local substring="$3"

    if [[ "$string" == *"$substring"* ]]
    then
        assertion_passed "$name contains $substring"
    else
        assertion_failed "$name ($string) does not contain $substring"
    fi
}

assert_equals_array () {
    local name="$1"
    shift
    local val="$1"
    shift
    local expected=("$@")
    if [ "${#val[@]}" == "${#expected[@]}" ]
    then
        assertion_passed "\${#${name}[@]} == ${#expected[@]}"
    else
        assertion_failed "\${#${name}[@]} == ${#expected[@]}"
    fi
}
