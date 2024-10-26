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
    if [ ${#FILE_RESULTS[@]} == 0 ]
    then
        echo -e "\nMatching directories: 0"
    else
        matching_dirs=()
        for r in ${FILE_RESULTS[*]}
        do
            df=( $(IFS=","; echo $r) )
            d=${df[0]}
            matching_dirs+=($d)
        done
        # matching_dirs=($(echo "${matching_dirs[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))
        matching_dirs=($(printf "%s\n" "${matching_dirs[@]}" | sort -u))

        echo -e "\nMatching directories (${#matching_dirs[@]}):"
        for d in ${matching_dirs[*]}
        do
            echo "$d"
        done
    fi
fi

if [ "$PRINT_FILES" == true ]
then
    if [ ${#FILE_RESULTS[@]} == 0 ]
    then
        echo -e "\nMatching files: 0"
    else
        matching_files=()
        for r in ${FILE_RESULTS[*]}
        do
            ra=( $(IFS=","; echo $r) )
            f="${ra[0]}/${ra[1]}"
            matching_files+=($f)
        done
        matching_files=($(printf "%s\n" "${matching_files[@]}"))
        echo -e "\nMatching files (${#matching_files[@]}):"
        for f in ${matching_files[@]}
        do
            echo "$f"
        done
    fi
fi
