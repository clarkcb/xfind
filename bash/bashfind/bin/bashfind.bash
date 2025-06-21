#!/bin/bash
################################################################################
#
# bashfind.bash
#
# A bash version of xfind
#
################################################################################

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

BASHFIND_PATH="$XFIND_PATH/bash/bashfind"

# Source the bashfind library
source "$BASHFIND_PATH/lib/bashfindlib.bash"

if [ $# == 0 ]
then
    exit_with_error "Startpath not defined"
else
    # Set to true since we're executing in shell
    PRINT_FILES=true
fi

settings_from_args $@

if [ "$DEBUG" == true ]
then
    s=$(settings_to_string)
    echo -e "\nSettings: $s"
fi

if [ "$PRINT_USAGE" == true ]
then
    usage
fi

if [ "$PRINT_VERSION" == true ]
then
    echo -e "bashfind 0.1"
    exit
fi

validate_settings

do_find

if [ "$PRINT_DIRS" == true ]
then
    print_file_results_dirs
fi

if [ "$PRINT_FILES" == true ]
then
    print_file_results
fi
