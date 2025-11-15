#!/bin/bash
################################################################################
#
# bashfindlib.bash
#
# Library for bash version of xfind
#
# Requirements:
# - bash
# - find
# - grep
# - jq
# - sed
# - sort
#
################################################################################

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

BASHFIND_PATH="$XFIND_PATH/bash/bashfind"
SHARED_PATH="$XFIND_PATH/shared"
FILE_TYPES_PATH="$XFIND_PATH/shared/filetypes.json"
FIND_OPTIONS_PATH="$XFIND_PATH/shared/findoptions.json"

# this will be contain the contents of FIND_OPTIONS_PATH if needed
FIND_OPTIONS_JSON=

source "$BASHFIND_PATH/lib/color.sh"


# File Types
FILE_TYPES=( $(jq -r '.filetypes[] | .type' $FILE_TYPES_PATH) )
FILE_TYPE_EXTENSIONS=()
FILE_TYPE_NAMES=()
FILE_TYPES_FOR_NAMES=()
for t in ${FILE_TYPES[*]}
do
    exts=( $(jq -r ".filetypes[] | select(.type == \"$t\") | .extensions[]" $FILE_TYPES_PATH) )
    jexts=$(IFS=','; echo "${exts[*]}")
    FILE_TYPE_EXTENSIONS+=($jexts)
    names=( $(jq -r ".filetypes[] | select(.type == \"$t\") | .names[]" $FILE_TYPES_PATH) )
    jnames=$(IFS=','; echo "${names[*]}")
    FILE_TYPE_NAMES+=($jnames)
    if [ ${#names[@]} -gt 0 ]
    then
        FILE_TYPES_FOR_NAMES+=($t)
    fi
done


# Sort By
SORT_BY_TYPES=(path name size type lastmod)


# Settings
ARCHIVES_ONLY=false
COLORIZE=true
DEBUG=false
FOLLOW_SYMLINKS=false
INCLUDE_ARCHIVES=false
INCLUDE_HIDDEN=false
IN_ARCHIVE_EXTENSIONS=()
IN_ARCHIVE_FILE_PATTERNS=()
IN_DIR_PATTERNS=()
IN_EXTENSIONS=()
IN_FILE_PATTERNS=()
IN_FILE_TYPES=()
MAX_DEPTH=-1
MAX_LAST_MOD=
MAX_LAST_MOD_EPOCH=0
MAX_SIZE=0
MIN_DEPTH=-1
MIN_LAST_MOD=
MIN_LAST_MOD_EPOCH=0
MIN_SIZE=0
NEED_FILE_SIZE=false
NEED_FILE_TYPE=false
NEED_LAST_MOD=false
OUT_ARCHIVE_EXTENSIONS=()
OUT_ARCHIVE_FILE_PATTERNS=()
OUT_DIR_PATTERNS=()
OUT_EXTENSIONS=()
OUT_FILE_PATTERNS=()
OUT_FILE_TYPES=()
PATHS=()
PRINT_DIRS=false
PRINT_FILES=false
PRINT_USAGE=false
PRINT_VERSION=false
RECURSIVE=true
SETTINGS_FILE=
SORT_BY=path
SORT_CASE_SENSITIVE=false
SORT_DESCENDING=false
VERBOSE=false

BOOL_OPTS=(archivesonly debug followsymlinks help includearchives includehidden printdirs printfiles recursive sort-casesensitive sort-descending verbose version)
NUM_OPTS=(maxdepth maxlastmod maxsize mindepth minlastmod minsize)
STR_OPTS=(in-archiveext in-archivefilepattern in-dirpattern in-ext in-filepattern in-filetype out-archiveext out-archivefilepattern out-dirpattern out-ext
          out-filepattern out-filetype path settings-file sort-by)

ARG_TYPES=(unknown bool num str)

FILE_RESULTS=()


exit_with_error () {
    local error="$1"
    local color="$BRED"
    if [ "$COLORIZE" == true ]
    then
        echo -e "\n${color}ERROR: $error${COLOR_RESET}" 1>&2
    else
        echo -e "\nERROR: $error"
    fi
    usage
}

get_arg_type () {
    local arg_name="$1"

    if [[ " ${BOOL_OPTS[*]} " =~ [[:space:]]${arg_name}[[:space:]] ]]
    then
        echo 'bool'
    else
        if [[ " ${STR_OPTS[*]} " =~ [[:space:]]${arg_name}[[:space:]] ]]
        then
            echo 'str'
        else
            if [[ " ${NUM_OPTS[*]} " =~ [[:space:]]${arg_name}[[:space:]] ]]
            then
                echo 'num'
            else
                echo 'unknown'
            fi
        fi
    fi
}

is_arg_type () {
    local arg_type="$1"
    local arg_name="$2"

    local atype=$(get_arg_type "$arg_name")
    if [ "$atype" == "$arg_type" ]
    then
        return 1
    fi
    return 0
}

is_file_type () {
    local file_type="$1"
    local file_path="$2"
    local file_name=$(basename $file_path)

    # Check names
    local name_idx=-1
    for i in ${!FILE_TYPES_FOR_NAMES[*]}
    do
        if [ "${FILE_TYPES_FOR_NAMES[$i]}" == "$file_type" ]
        then
            name_idx=$i
            break
        fi
    done
    if [ $name_idx -gt -1 ]
    then
        if [[ ",${FILE_TYPE_NAMES[$name_idx]}," =~ ",$file_name," ]]
        then
            return 1
        fi
    fi

    # Check extensions
    local ext_idx=-1
    for i in ${!FILE_TYPES[*]}
    do
        if [ "${FILE_TYPES[$i]}" == "$file_type" ]
        then
            ext_idx=$i
            break
        fi
    done

    if [ $ext_idx -gt -1 ]
    then
        local file_ext="${file_name##*.}"
        if [[ ",${FILE_TYPE_EXTENSIONS[$ext_idx]}," =~ ",$file_ext," ]]
        then
            return 1
        fi
    fi

    if [ "$file_type" == "unknown" ]
    then
        return 1
    fi

    return 0
}

get_file_type () {
    local file_path="$1"

    # start with specific file types
    specific_types=(code archive audio font image video)
    for t in "${specific_types[@]}"
    do
        is_file_type "$t" "$file_path"
        if [ $? == 1 ]
        then
            echo "$t"
            return
        fi
    done

    # end with general file types
    general_types=(text binary xml)
    for t in "${general_types[@]}"
    do
        is_file_type "$t" "$file_path"
        if [ $? == 1 ]
        then
            echo "$t"
            return
        fi
    done

    echo 'unknown'
}

validate_settings () {
    if [ ${#PATHS[@]} == 0 ]
    then
        exit_with_error "Startpath not defined"
    fi

    for p in ${PATHS[*]}
    do
        if [ ! -d $p -a ! -f $p ]
        then
            p=$(eval echo "$p")
            if [ ! -d $p -a ! -f $p ]
            then
                exit_with_error "Startpath not found"
            fi
        fi
        if [ ! -r $p ]
        then
            p=$(eval echo "$p")
            if [ ! -r $p ]
            then
                exit_with_error "Startpath not readable"
            fi
        fi
    done

    if [ $MAX_DEPTH -gt -1 -a $MIN_DEPTH -gt -1 -a $MAX_DEPTH -lt $MIN_DEPTH ]
    then
        exit_with_error "Invalid range for mindepth and maxdepth"
    fi

    if [ $MAX_LAST_MOD_EPOCH -gt 0 -a $MIN_LAST_MOD_EPOCH -gt 0 -a $MAX_LAST_MOD_EPOCH -lt $MIN_LAST_MOD_EPOCH ]
    then
        exit_with_error "Invalid range for minlastmod and maxlastmod"
    fi

    if [ $MAX_SIZE -gt 0 -a $MIN_SIZE -gt 0 -a $MAX_SIZE -lt $MIN_SIZE ]
    then
        exit_with_error "Invalid range for minsize and maxsize"
    fi
}

is_dot_dir () {
    local dir_path="$1"
    if [[ "$dir_path" =~ ^\.{1,2}[/\\]?$ ]]
    then
        return 1
    fi
    return 0
}

is_hidden_name () {
    local name="$1"

    if [[ "${name:0:1}" == "." ]]
    then
        is_dot_dir "$elem"
        if [ $? == 0 ]
        then
            return 1
        fi
    fi

    return 0
}

is_hidden_path () {
    local file_path="$1"

    IFS="/" read -ra elems <<< "$file_path"

    for elem in "${elems[@]}"; do
        # echo "$elem"
        is_hidden_name "$elem"
        if [ $? == 1 ]
        then
            return 1
        fi
    done

    return 0
}

filter_dir_by_hidden () {
    local dir_path="$1"

    if [ "$dir_path" == "" ]
    then
        return 1
    fi

    is_dot_dir "$dir_path"
    if [ $? == 1 ]
    then
        return 1
    fi

    if [ $INCLUDE_HIDDEN == false ]
    then
        is_hidden_path "$dir_path"
        if [ $? == 1 ]
        then
            return 0
        fi
    fi

    return 1
}

filter_dir_by_in_patterns () {
    local dir_path="$1"

    if [ ${#IN_DIR_PATTERNS[@]} -gt 0 ]
    then
        for p in ${IN_DIR_PATTERNS[*]}
        do
            if [[ "$dir_path" =~ $p ]]
            then
                return 1
            fi
        done
        return 0
    fi

    return 1
}

filter_dir_by_out_patterns () {
    local dir_path="$1"

    if [ ${#OUT_DIR_PATTERNS[@]} -gt 0 ]
    then
        for p in ${OUT_DIR_PATTERNS[*]}
        do
            if [[ "$dir_path" =~ $p ]]
            then
                return 0
            fi
        done
        # return 0
    fi

    return 1
}

is_matching_dir () {
    local dir_path="$1"

    is_dot_dir "$dir_path"
    if [ $? == 1 ]
    then
        return 1
    fi

    filter_dir_by_hidden "$dir_path"
    if [ $? == 0 ]
    then
        return 0
    fi

    filter_dir_by_in_patterns "$dir_path"
    if [ $? == 0 ]
    then
        return 0
    fi

    filter_dir_by_out_patterns "$dir_path"
    if [ $? == 0 ]
    then
        return 0
    fi

    return 1
}

has_matching_archive_ext () {
    local file_name="$1"

    if [ ${#IN_ARCHIVE_EXTENSIONS[@]} -gt 0 -o ${#OUT_ARCHIVE_EXTENSIONS[@]} -gt 0 ]
    then
        local ext="${file_name##*.}"
        local found_in_ext=0
        if [ ${#IN_ARCHIVE_EXTENSIONS[@]} -gt 0 ]
        then
            for x in ${IN_ARCHIVE_EXTENSIONS[*]}
            do
                if [[ "$ext" = "$x" ]]
                then
                    found_in_ext=1
                fi
            done
            if [ $found_in_ext == 0 ]
            then
                return 0
            fi
        fi

        if [ ${#OUT_ARCHIVE_EXTENSIONS[@]} -gt 0 ]
        then
            for x in ${OUT_ARCHIVE_EXTENSIONS[*]}
            do
                if [[ "$ext" = "$x" ]]
                then
                    return 0
                fi
            done
        fi
    fi

    return 1
}

is_matching_archive_file_name () {
    local file_name="$1"

    if [ ${#IN_ARCHIVE_FILE_PATTERNS[@]} -gt 0 -o ${#OUT_ARCHIVE_FILE_PATTERNS[@]} -gt 0 ]
    then
        local file_name_no_ext="${file_name%.*}"
        if [ ${#IN_ARCHIVE_FILE_PATTERNS[@]} -gt 0 ]
        then
            local found_in_pat=0
            for p in ${IN_ARCHIVE_FILE_PATTERNS[*]}
            do
                if [[ "$file_name_no_ext" =~ $p ]]
                then
                    found_in_pat=1
                fi
            done
            if [ $found_in_pat == 0 ]
            then
                return 0
            fi
        fi

        if [ ${#OUT_ARCHIVE_FILE_PATTERNS[@]} -gt 0 ]
        then
            for p in ${OUT_ARCHIVE_FILE_PATTERNS[*]}
            do
                if [[ "$file_name_no_ext" =~ $p ]]
                then
                    return 0
                fi
            done
        fi
    fi

    return 1
}

is_matching_archive_file () {
    local file_path="$1"
    local file_name=$(basename $file_path)

    if [ $INCLUDE_HIDDEN == false ]
    then
        is_hidden_name "$file_name"
        if [ $? == 1 ]
        then
            return 0
        fi
    fi

    has_matching_archive_ext "$file_name"
    if [ $? == 0 ]
    then
        return 0
    fi

    is_matching_archive_file_name "$file_name"
    if [ $? == 0 ]
    then
        return 0
    fi

    return 1
}

has_matching_ext () {
    local file_name="$1"

    if [ ${#IN_EXTENSIONS[@]} -gt 0 -o ${#OUT_EXTENSIONS[@]} -gt 0 ]
    then
        local file_ext="${file_name##*.}"
        if [ ${#IN_EXTENSIONS[@]} -gt 0 ]
        then
            local found_in_ext=0
            for x in ${IN_EXTENSIONS[*]}
            do
                if [[ "$file_ext" = "$x" ]]
                then
                    found_in_ext=1
                fi
            done
            if [ $found_in_ext == 0 ]
            then
                return 0
            fi
        fi

        if [ ${#OUT_EXTENSIONS[@]} -gt 0 ]
        then
            for x in ${OUT_EXTENSIONS[*]}
            do
                if [[ "$file_ext" = "$x" ]]
                then
                    return 0
                fi
            done
        fi
    fi

    return 1
}

is_matching_file_name () {
    local file_name="$1"

    if [ ${#IN_FILE_PATTERNS[@]} -gt 0 -o ${#OUT_FILE_PATTERNS[@]} -gt 0 ]
    then
        local file_name_no_ext="${file_name%.*}"
        if [ ${#IN_FILE_PATTERNS[@]} -gt 0 ]
        then
            local found_in_pat=0
            for p in ${IN_FILE_PATTERNS[*]}
            do
                if [[ "$file_name_no_ext" =~ $p ]]
                then
                    found_in_pat=1
                fi
            done
            if [ $found_in_pat == 0 ]
            then
                return 0
            fi
        fi

        if [ ${#OUT_FILE_PATTERNS[@]} -gt 0 ]
        then
            for p in ${OUT_FILE_PATTERNS[*]}
            do
                if [[ "$file_name_no_ext" =~ $p ]]
                then
                    return 0
                fi
            done
        fi
    fi

    return 1
}

is_matching_file_type () {
    local file_type="$1"

    if [ ${#IN_FILE_TYPES[@]} -gt 0 ]
    then
        local found_in_type=0
        for t in ${IN_FILE_TYPES[*]}
        do
            if [[ "$file_type" == "$t" ]]
            then
                found_in_type=1
            fi
        done
        if [ $found_in_type == 0 ]
        then
            return 0
        fi
    fi

    if [ ${#OUT_FILE_TYPES[@]} -gt 0 ]
    then
        for t in ${OUT_FILE_TYPES[*]}
        do
            if [[ "$file_type" = "$t" ]]
            then
                return 0
            fi
        done
    fi

    return 1
}

is_matching_file_size () {
    local file_size="$1"

    if [ $MAX_SIZE -gt 0 ]
    then
        if [ $file_size -gt $MAX_SIZE ]
        then
            return 0
        fi
    fi

    if [ $MIN_SIZE -gt 0 ]
    then
        if [ $file_size -lt $MIN_SIZE ]
        then
            return 0
        fi
    fi

    return 1
}

is_matching_last_mod () {
    local last_mod="$1"

    if [ $MAX_LAST_MOD_EPOCH -gt 0 ]
    then
        if [ $last_mod -gt $MAX_LAST_MOD_EPOCH ]
        then
            return 0
        fi
    fi

    if [ $MIN_LAST_MOD_EPOCH -gt 0 ]
    then
        if [ $last_mod -lt $MIN_LAST_MOD_EPOCH ]
        then
            return 0
        fi
    fi

    return 1
}

is_matching_file () {
    local file_path="$1"
    local file_type="$2"
    local file_size=$3
    local last_mod=$4
    local file_name=$(basename $file_path)

    if [ $INCLUDE_HIDDEN == false ]
    then
        is_hidden_name "$file_name"
        if [ $? == 1 ]
        then
            return 0
        fi
    fi

    has_matching_ext "$file_name"
    if [ $? == 0 ]
    then
        return 0
    fi

    is_matching_file_name "$file_name"
    if [ $? == 0 ]
    then
        return 0
    fi

    is_matching_file_type "$file_type"
    if [ $? == 0 ]
    then
        return 0
    fi

    is_matching_file_size $file_size
    if [ $? == 0 ]
    then
        return 0
    fi

    is_matching_last_mod $last_mod
    if [ $? == 0 ]
    then
        return 0
    fi

    return 1
}

filter_to_file_result () {
    local file_path="$1"
    local dir_path=$(dirname $file_path)
    local file_name=$(basename $file_path)
    local file_type='unknown'
    local file_size=0
    local last_mod=0

    is_matching_dir "$dir_path"
    if [ $? == 0 ]
    then
        return
    fi

    if [ $INCLUDE_HIDDEN == false ]
    then
        is_hidden_name "$file_name"
        if [ $? == 1 ]
        then
            return
        fi
    fi

    is_file_type 'archive' "$file_name"
    local archive_match=$?
    if [ "$archive_match" == 1 ]
    then
        file_type='archive'
        if [ $INCLUDE_ARCHIVES == true ]
        then
            is_matching_archive_file "$file_name"
            local file_match=$?
            if [ "$file_match" == 1 ]
            then
                FILE_RESULTS+=("$dir_path,$file_name,$file_type,$file_size,$last_mod")
            fi
        fi
    else
        if [ $NEED_FILE_TYPE == true ]
        then
            file_type=$(get_file_type $file_path)
        fi
        if [ $NEED_FILE_SIZE == true ]
        then
            file_size=$(stat -f %z $file_path)
        fi
        if [ $NEED_LAST_MOD == true ]
        then
            last_mod=$(stat -f %m $file_path)
        fi
        is_matching_file "$file_name" "$file_type" $file_size $last_mod
        if [ $? == 1 ]
        then
            FILE_RESULTS+=("$dir_path,$file_name,$file_type,$file_size,$last_mod")
        fi
    fi
}

filter_to_file_results () {
    local file_paths=("$@")

    for file_path in ${file_paths[*]}
    do
        filter_to_file_result $file_path
    done
}

rec_find_path () {
    local path="$1"
    local current_depth=$2

    # remove trailing slash if present
    path="${path%/}" 

    if [ $MAX_DEPTH -gt -1 -a $current_depth -gt $MAX_DEPTH ]
    then
        return 0
    fi

    local recurse=true
    if [ $current_depth == $MAX_DEPTH ]
    then
        recurse=false
    fi

    local path_dirs=()
    local path_files=()
    local primary_options=""
    if [ "$FOLLOW_SYMLINKS" == true ]
    then
        primary_options="-L"
    else
        primary_options="-P"
    fi
    # optimization levels 1-3 from lowest to highest
    # NOTE: this does not work on macOS's version of find
    # level_option="-O2"
    other_options="-maxdepth 1"
    if [ "$recurse" == true ]
    then
        path_dirs=$(find $primary_options $path -type d $other_options | grep -v "^$path$")
    fi
    if [ $MIN_DEPTH -lt 0 -o $current_depth -ge $MIN_DEPTH ]
    then
        if [ $MAX_SIZE -gt 0 ]
        then
            other_options+=" -size -${MAX_SIZE}c"
        fi
        if [ $MIN_SIZE -gt 0 ]
        then
            other_options+=" -size +${MIN_SIZE}c"
        fi
        path_files=$(find $primary_options $path -type f $other_options)
    fi

    filter_to_file_results "${path_files[*]}"

    for d in ${path_dirs[*]}
    do
        filter_dir_by_hidden "$d"
        if [ $? == 0 ]
        then
            continue
        fi

        filter_dir_by_out_patterns "$d"
        if [ $? == 0 ]
        then
            continue
        fi

        rec_find_path $d $(($current_depth + 1))
    done
}

find_path () {
    local path="$1"

    if [ ! -e "$path" ]
    then
        # this should result in an expanded path
        path=$(eval echo "$path")
    fi

    if [ -d "$path" ]
    then
        if [ $MAX_DEPTH -eq 0 ]
        then
            return
        fi

        if [ $RECURSIVE == false ]
        then
            MAX_DEPTH=1
        fi

        filter_dir_by_hidden "$d"
        if [ $? == 0 ]
        then
            # TODO: report error that path does not match settings
            return
        fi

        filter_dir_by_out_patterns "$d"
        if [ $? == 0 ]
        then
            # TODO: report error that path does not match settings
            return
        fi

        rec_find_path $path 1

    else
        if [ -f "$path" ]
        then
            local file_name=$(basename $path)
            local file_type='unknown'
            local file_size=0
            local last_mod=0

            need_file_type
            if [ $? == 1 ]
            then
                file_type=$(get_file_type $path)
            fi

            is_matching_file "$path" "$file_type" $file_size $last_mod
            local file_match=$?
            if [ "$file_match" == 1 ]
            then
                local dir_path=$(dirname $path)
                local file_name=$(basename $path)
                FILE_RESULTS+=("$dir_path,$file_name")
            fi
        fi
    fi
}

format_dir_path () {
    local dir_path="$1"

    formatted_dir_path="$dir_path"
    if [[ "$COLORIZE" == true && ${#IN_DIR_PATTERNS[@]} -gt 0 ]]
    then
        for p in ${IN_DIR_PATTERNS[*]}
        do
            if [[ "$dir_path" =~ $p ]]
            then
                match="${BASH_REMATCH[0]}"
                formatted_dir_path="${dir_path//$match/${GREEN}$match${COLOR_RESET}}"
                break
            fi
        done
    fi
    echo "$formatted_dir_path"
}

format_file_name () {
    local file_name="$1"
    local formatted_file_name="$file_name"

    if [[ "$COLORIZE" == true && (${#IN_EXTENSIONS[@]} -gt 0 || ${#IN_FILE_PATTERNS[@]} -gt 0) ]]
    then
        for p in ${IN_FILE_PATTERNS[*]}
        do
            if [[ "$file_name" =~ $p ]]
            then
                match="${BASH_REMATCH[0]}"
                formatted_file_name="${file_name//$match/${GREEN}$match${COLOR_RESET}}"
                break
            fi
        done

        if [ ${#IN_EXTENSIONS[@]} -gt 0 ]
        then
            local file_ext="${formatted_file_name##*.}"
            local formatted_no_ext="${formatted_file_name%.*}"
            formatted_file_name="${formatted_no_ext}.${GREEN}$file_ext${COLOR_RESET}"
        fi
    fi
    echo "$formatted_file_name"
}


format_file_result () {
    local r="$1"

    ra=( $(IFS=","; echo $r) )

    dir_path="${ra[0]}"
    dir_path=$(format_dir_path "$dir_path")

    file_name="${ra[1]}"
    file_name=$(format_file_name "$file_name")

    formatted_file_path="$dir_path/$file_name"
    echo "$formatted_file_path"
}


file_results_to_string () {
    local s=""
    for r in ${FILE_RESULTS[*]}
    do
        s+="$r\n"
    done
    echo -e "$s"
}

reverse_file_results () {
    local reversed=()
    for (( i=${#FILE_RESULTS[@]}-1; i>=0; i-- ))
    do
        reversed+=(${FILE_RESULTS[$i]})
    done
    FILE_RESULTS=("${reversed[@]}")
}

sort_file_results () {
    local sort_by=$1
    local sort_descending=$2
    local sort_case_sensitive=$3
    local sort_cmd="sort -t,"
    if [ "$sort_case_sensitive" == true ]
    then
        sort_cmd+=" -f"
    fi
    fallback="-k1,1 -k2,2"
    case "$sort_by" in
        name)
            sort_cmd+=" -k2,2 -k1,1"
            ;;
        size)
            sort_cmd+=" -k4,4n $fallback"
            ;;
        type)
            sort_cmd+=" -k3,3 $fallback"
            ;;
        lastmod)
            sort_cmd+=" -k5,5n $fallback"
            ;;
        *)
            sort_cmd+=" $fallback"
            ;;
    esac
    # Using the -r option for sort doesn't seem to work, doing manual reverse instead
    # if [ "$sort_descending" == true ]
    # then
    #     sort_cmd+=" -r"
    # fi
    FILE_RESULTS=($(echo "${FILE_RESULTS[*]}" | tr ' ' '\n' | $sort_cmd | tr '\n' ' '))
    if [ "$sort_descending" == true ]
    then
        reverse_file_results
    fi
}

do_find () {
    for p in ${PATHS[*]}
    do
        find_path $p
    done
    if [ ${#FILE_RESULTS[@]} -gt 1 ]
    then
        sort_file_results $SORT_BY $SORT_DESCENDING $SORT_CASE_SENSITIVE
    fi
}

print_file_results_dirs () {
    if [ ${#FILE_RESULTS[@]} == 0 ]
    then
        echo -e "\nMatching directories: 0"
    else
        matching_dirs=()
        for r in ${FILE_RESULTS[*]}
        do
            df=( $(IFS=","; echo $r) )
            d=${df[0]}
            d=$(format_dir_path "$d")
            matching_dirs+=($d)
        done
        matching_dirs=($(printf "%s\n" "${matching_dirs[@]}" | sort -u))

        echo -e "\nMatching directories (${#matching_dirs[@]}):"
        for d in ${matching_dirs[*]}
        do
            echo -e "$d"
        done
    fi
}

print_file_results () {
    if [ ${#FILE_RESULTS[@]} == 0 ]
    then
        echo -e "\nMatching files: 0"
    else
        matching_files=()
        for r in ${FILE_RESULTS[*]}
        do
            f=$(format_file_result "$r")
            matching_files+=($f)
        done
        matching_files=($(printf "%s\n" "${matching_files[@]}"))
        echo -e "\nMatching files (${#matching_files[@]}):"
        for f in ${matching_files[@]}
        do
            echo -e "$f"
        done
    fi
}

get_option_desc_from_file () {
    local long=$1
    local desc=$(jq -r ".findoptions[] | select(.long == \"$long\") | .desc" $FIND_OPTIONS_PATH)
    echo $desc
}

get_option_desc () {
    local long=$1
    local desc=$(echo "$FIND_OPTIONS_JSON" | jq -r ".findoptions[] | select(.long == \"$long\") | .desc")
    echo $desc
}

usage () {
    FIND_OPTIONS_JSON=$(cat $FIND_OPTIONS_PATH)
    s="\nUsage:\n bashfind [options] <path> [<path> ...]\n\nOptions:"
    s+="\n --archivesonly            $(get_option_desc 'archivesonly')"
    s+="\n -c,--colorize             $(get_option_desc 'colorize')"
    s+="\n -C,--nocolorize           $(get_option_desc 'nocolorize')"
    s+="\n -d,--in-dirpattern        $(get_option_desc 'in-dirpattern')"
    s+="\n -D,--out-dirpattern       $(get_option_desc 'out-dirpattern')"
    s+="\n --debug                   $(get_option_desc 'debug')"
    s+="\n --excludehidden           $(get_option_desc 'excludehidden')"
    s+="\n --followsymlinks          $(get_option_desc 'followsymlinks')"
    s+="\n -f,--in-filepattern       $(get_option_desc 'in-filepattern')"
    s+="\n -F,--out-filepattern      $(get_option_desc 'out-filepattern')"
    s+="\n -h,--help                 $(get_option_desc 'help')"
    s+="\n --includehidden           $(get_option_desc 'includehidden')"
    s+="\n --in-archiveext           $(get_option_desc 'in-archiveext')"
    s+="\n --in-archivefilepattern   $(get_option_desc 'in-archivefilepattern')"
    s+="\n --maxdepth                $(get_option_desc 'maxdepth')"
    s+="\n --maxlastmod              $(get_option_desc 'maxlastmod')"
    s+="\n --maxsize                 $(get_option_desc 'maxsize')"
    s+="\n --mindepth                $(get_option_desc 'mindepth')"
    s+="\n --minlastmod              $(get_option_desc 'minlastmod')"
    s+="\n --minsize                 $(get_option_desc 'minsize')"
    s+="\n --nofollowsymlinks        $(get_option_desc 'nofollowsymlinks')"
    s+="\n --noprintdirs             $(get_option_desc 'noprintdirs')"
    s+="\n --noprintfiles            $(get_option_desc 'noprintfiles')"
    s+="\n --out-archiveext          $(get_option_desc 'out-archiveext')"
    s+="\n --out-archivefilepattern  $(get_option_desc 'out-archivefilepattern')"
    s+="\n --path                    $(get_option_desc 'path')"
    s+="\n --printdirs               $(get_option_desc 'printdirs')"
    s+="\n --printfiles              $(get_option_desc 'printfiles')"
    s+="\n -R,--norecursive          $(get_option_desc 'norecursive')"
    s+="\n -r,--recursive            $(get_option_desc 'recursive')"
    s+="\n --settings-file           $(get_option_desc 'settings-file')"
    s+="\n --sort-ascending          $(get_option_desc 'sort-ascending')"
    s+="\n --sort-by                 $(get_option_desc 'sort-by')"
    s+="\n --sort-caseinsensitive    $(get_option_desc 'sort-caseinsensitive')"
    s+="\n --sort-casesensitive      $(get_option_desc 'sort-casesensitive')"
    s+="\n --sort-descending         $(get_option_desc 'sort-descending')"
    s+="\n -t,--in-filetype          $(get_option_desc 'in-filetype')"
    s+="\n -T,--out-filetype         $(get_option_desc 'out-filetype')"
    s+="\n -v,--verbose              $(get_option_desc 'verbose')"
    s+="\n -V,--version              $(get_option_desc 'version')"
    s+="\n -x,--in-ext               $(get_option_desc 'in-ext')"
    s+="\n -X,--out-ext              $(get_option_desc 'out-ext')"
    s+="\n -Z,--excludearchives      $(get_option_desc 'excludearchives')"
    s+="\n -z,--includearchives      $(get_option_desc 'includearchives')"
    echo -e "$s\n"
    exit
}

array_to_string () {
    local arr=("$@")
    local s="["
    local i=0
    for a in ${arr[*]}
    do
        if [ $i -gt 0 ]
        then
            s+=","
        fi
        s+="$a"
        i=$(($i + 1))
    done
    s+="]"
    echo $s
}

arg_wants_val () {
    local arg="$1"

    case "$arg" in
        --in-archiveext)
            return 1
            ;;
        --in-archivefilepattern)
            return 1
            ;;
        -d | --in-dirpattern)
            return 1
            ;;
        -x | --in-ext)
            return 1
            ;;
        -f | --in-filepattern)
            return 1
            ;;
        =t | --in-filetype)
            return 1
            ;;
        --maxdepth)
            return 1
            ;;
        --maxlastmod)
            return 1
            ;;
        --maxsize)
            return 1
            ;;
        --mindepth)
            return 1
            ;;
        --minlastmod)
            return 1
            ;;
        --minsize)
            return 1
            ;;
        --out-archiveext)
            return 1
            ;;
        --out-archivefilepattern)
            return 1
            ;;
        -D | --out-dirpattern)
            return 1
            ;;
        -X | --out-ext)
            return 1
            ;;
        -F | --out-filepattern)
            return 1
            ;;
        -T | --out-filetype)
            return 1
            ;;
        --path)
            return 1
            ;;
        --settings-file)
            return 1
            ;;
        --sort-by)
            return 1
            ;;
    esac

    return 0
}

settings_from_args () {
    local args=("$@")
    local i=0

    while [ $i -lt ${#args[*]} ]
    do
        local arg=${args[$i]}
        local arg2=
        if [ $(($i + 1)) -lt ${#args[*]} ]
        then
            arg2=${args[$(($i + 1))]}
        fi
        case "$arg" in
            --archivesonly)
                ARCHIVES_ONLY=true
                INCLUDE_ARCHIVES=true
                ;;
            -c | --colorize)
                COLORIZE=true
                ;;
            --debug)
                DEBUG=true
                VERBOSE=true
                ;;
            -Z | --excludearchives)
                INCLUDE_ARCHIVES=false
                ;;
            --excludehidden)
                INCLUDE_HIDDEN=false
                ;;
            --followsymlinks)
                FOLLOW_SYMLINKS=true
                ;;
            -h | --help)
                PRINT_USAGE=true
                ;;
            -z | --includearchives)
                INCLUDE_ARCHIVES=true
                ;;
            --includehidden)
                INCLUDE_HIDDEN=true
                ;;
            --in-archiveext)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                IFS=',' read -r -a exts <<< "$arg2"
                for e in ${exts[*]}
                do
                    IN_ARCHIVE_EXTENSIONS+=($e)
                done
                i=$(($i + 1))
                ;;
            --in-archivefilepattern)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                IN_ARCHIVE_FILE_PATTERNS+=($arg2)
                i=$(($i + 1))
                ;;
            -d | --in-dirpattern)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                IN_DIR_PATTERNS+=($arg2)
                i=$(($i + 1))
                ;;
            -x | --in-ext)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                IFS=',' read -r -a exts <<< "$arg2"
                for e in ${exts[*]}
                do
                    IN_EXTENSIONS+=($e)
                done
                i=$(($i + 1))
                ;;
            -f | --in-filepattern)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                IN_FILE_PATTERNS+=($arg2)
                i=$(($i + 1))
                ;;
            -t | --in-filetype)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                local file_type=$(echo $arg2 | tr '[:upper:]' '[:lower:]')
                if [[ ! " ${FILE_TYPES[*]} " =~ [[:space:]]$file_type[[:space:]] ]]
                then
                    file_type=unknown
                fi
                IN_FILE_TYPES+=($file_type)
                NEED_FILE_TYPE=true
                i=$(($i + 1))
                ;;
            --maxdepth)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                MAX_DEPTH=$arg2
                i=$(($i + 1))
                ;;
            --maxlastmod)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                MAX_LAST_MOD=$arg2
                MAX_LAST_MOD_EPOCH=$(date -j -f "%Y-%m-%d" $MAX_LAST_MOD "+%s")
                NEED_LAST_MOD=true
                i=$(($i + 1))
                ;;
            --maxsize)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                MAX_SIZE=$arg2
                NEED_FILE_SIZE=true
                i=$(($i + 1))
                ;;
            --mindepth)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                MIN_DEPTH=$arg2
                i=$(($i + 1))
                ;;
            --minlastmod)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                MIN_LAST_MOD=$arg2
                MIN_LAST_MOD_EPOCH=$(date -j -f "%Y-%m-%d" $MIN_LAST_MOD "+%s")
                NEED_LAST_MOD=true
                i=$(($i + 1))
                ;;
            --minsize)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                MIN_SIZE=$arg2
                NEED_FILE_SIZE=true
                i=$(($i + 1))
                ;;
            -C | --nocolorize)
                COLORIZE=false
                ;;
            --nodebug)
                DEBUG=false
                ;;
            --nofollowsymlinks)
                FOLLOW_SYMLINKS=false
                ;;
            --noprintdirs)
                PRINT_DIRS=false
                ;;
            --noprintfiles)
                PRINT_FILES=false
                ;;
            -R | --norecursive)
                RECURSIVE=false
                ;;
            --out-archiveext)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                IFS=',' read -r -a exts <<< "$arg2"
                for e in ${exts[*]}
                do
                    OUT_ARCHIVE_EXTENSIONS+=($e)
                done
                i=$(($i + 1))
                ;;
            --out-archivefilepattern)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                OUT_ARCHIVE_FILE_PATTERNS+=($arg2)
                i=$(($i + 1))
                ;;
            -D | --out-dirpattern)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                OUT_DIR_PATTERNS+=($arg2)
                i=$(($i + 1))
                ;;
            -X | --out-ext)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                IFS=',' read -r -a exts <<< "$arg2"
                for e in ${exts[*]}
                do
                    OUT_EXTENSIONS+=($e)
                done
                i=$(($i + 1))
                ;;
            -F | --out-filepattern)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                OUT_FILE_PATTERNS+=($arg2)
                i=$(($i + 1))
                ;;
            -T | --out-filetype)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                OUT_FILE_TYPES+=($arg2)
                NEED_FILE_TYPE=true
                i=$(($i + 1))
                ;;
            --path)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                PATHS+=($arg2)
                i=$(($i + 1))
                ;;
            --printdirs)
                PRINT_DIRS=true
                ;;
            --printfiles)
                PRINT_FILES=true
                ;;
            -r | --recursive)
                RECURSIVE=true
                ;;
            --settings-file)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                # SETTINGS_FILE="$arg2"
                update_settings_from_file $arg2
                i=$(($i + 1))
                ;;
            --sort-ascending)
                SORT_DESCENDING=false
                ;;
            --sort-by)
                if [ -z "$arg2" ]
                then
                    exit_with_error "Missing argument for option $arg"
                fi
                SORT_BY=$(echo $arg2 | tr '[:upper:]' '[:lower:]')
                i=$(($i + 1))
                ;;
            --sort-caseinsensitive)
                SORT_CASE_SENSITIVE=false
                ;;
            --sort-casesensitive)
                SORT_CASE_SENSITIVE=true
                ;;
            --sort-descending)
                SORT_DESCENDING=true
                ;;
            -v | --verbose)
                VERBOSE=true
                ;;
            -V | --version)
                PRINT_VERSION=true
                ;;
            --*)
                no_dash_arg=${arg:2}
                # If arg contains = then split into arg and arg2
                if [[ "$arg" == *=* ]]
                then
                    IFS='=' read -ra ARG_VAL <<< "$arg"
                    if [ ${#ARG_VAL[@]} -eq 2 ]
                    then
                        settings_from_args ${ARG_VAL[*]}
                    else
                        exit_with_error "Invalid option: $no_dash_arg"
                    fi
                else
                    exit_with_error "Invalid option: $no_dash_arg"
                fi
                ;;
            -*)
                arg=${arg:1}
                if [ ${#arg} -eq 1 ]
                then
                    exit_with_error "Invalid option: $arg"
                fi
                SHORT_ARGS=()
                while [ ${#arg} -gt 0 ]
                do
                    local short_arg="-${arg:0:1}"
                    SHORT_ARGS+=($short_arg)
                    arg=${arg:1}
                done
                # Check last elem with arg_wants_val
                last_elem="${SHORT_ARGS[@]: -1}"
                arg_wants_val $last_elem
                wants_val=$?
                if [ $wants_val -eq 1 ]
                then
                    if [ -z "$arg2" ]
                    then
                        exit_with_error "Missing argument for option $last_elem"
                    fi
                    SHORT_ARGS+=($arg2)
                    i=$(($i + 1))
                fi
                settings_from_args ${SHORT_ARGS[*]}
                ;;
            *)
                PATHS+=($arg)
                ;;
        esac
        i=$(($i + 1))
    done
    # SORT_BY_TYPES=(path name size type lastmod)
    case "$SORT_BY" in
        filename | name)
            SORT_BY=name
            ;;
        filesize | size)
            SORT_BY=size
            NEED_FILE_SIZE=true
            ;;
        filetype | type)
            SORT_BY=type
            NEED_FILE_TYPE=true
            ;;
        lastmod)
            NEED_LAST_MOD=true
            ;;
        *)
            SORT_BY=path
            ;;
    esac
}

negate_key () {
    local key="$1"
    case "$key" in
        include*)
            echo "exclude${key:7}"
            ;;
        exclude*)
            echo "include${key:7}"
            ;;
        no*)
            echo "${key:2}"
            ;;
        *)
            echo "no$key"
            ;;
    esac
}

update_settings_from_json () {
    local json="$1"
    local array_keys=( $(echo "$json" | jq -S -r 'with_entries(select(.value | type == "array")) | keys | .[]') )
    for k in ${array_keys[*]}
    do
        # Only string array options are supported
        if [[ " ${STR_OPTS[*]} " =~ [[:space:]]${k}[[:space:]] ]]
        then
            local arr=( $(echo "$json" | jq -r ".\"$k\" | .[]") )
            local arr_args=()
            for a in ${arr[*]}
            do
                arr_args+=("--$k")
                arr_args+=("$a")
            done
            settings_from_args ${arr_args[*]}
        else
            exit_with_error "Invalid option: $k"
        fi
    done
    local other_keys=( $(echo "$json" | jq -S -r 'with_entries(select(.value | type != "array")) | keys | .[]') )
    local other_args=()
    for k in ${other_keys[*]}
    do
        local arg_type=$(get_arg_type $k)
        if [ "$arg_type" == "unknown" ]
        then
            exit_with_error "Invalid option: $k"
        else
            local v=$(echo "$json" | jq -r ".\"$k\"")
            if [ "$arg_type" == "bool" ]
            then
                if [ "$v" == "true" -o "$v" == "false" ]
                then
                    if [ "$v" == "true" ]
                    then
                        other_args+=("--$k")
                    else
                        k=$(negate_key $k)
                        other_args+=("--$k")
                    fi
                else
                    exit_with_error "Invalid value for option: $k"
                fi
            else
                if [ "$arg_type" == "num" ]
                then
                    if [[ -n "$v" ]] && [[ -z "${v//[0-9]}" ]]
                    then
                        other_args+=("--$k")
                        other_args+=("$v")
                    else
                        exit_with_error "Invalid value for option: $k"
                    fi
                else
                    if [ "$arg_type" == "str" ]
                    then
                        # we can't really determine whether value is a string, so we just assume it is
                        other_args+=("--$k")
                        other_args+=("$v")
                    else
                        exit_with_error "Invalid option: $k"
                    fi
                fi
            fi
        fi
    done
    if [ ${#other_args[@]} -gt 0 ]
    then
        settings_from_args ${other_args[*]}
    fi
}

update_settings_from_file () {
    local file_path="$1"
    local file_name=$(basename $file_path)
    local file_ext="${file_name##*.}"

    expanded_path=$(eval echo "$file_path")
    if [ ! -f "$expanded_path" ]
    then
        exit_with_error "Settings file not found: $file_path"
    fi
    if [ ! "$file_ext" == "json" ]
    then
        exit_with_error "Invalid settings file (must be JSON): $file_path"
    fi

    json=$(cat $expanded_path)
    update_settings_from_json "$json"
}

settings_to_string () {
    s="archives_only=$ARCHIVES_ONLY"
    s+=", colorize=$COLORIZE"
    s+=", debug=$DEBUG"
    s+=", follow_symlinks=$FOLLOW_SYMLINKS"
    s+=", include_archives=$INCLUDE_ARCHIVES"
    s+=", include_hidden=$INCLUDE_HIDDEN"
    s+=", in_archive_extensions=$(array_to_string ${IN_ARCHIVE_EXTENSIONS[*]})"
    s+=", in_archive_file_patterns=$(array_to_string ${IN_ARCHIVE_FILE_PATTERNS[*]})"
    s+=", in_dir_patterns=$(array_to_string ${IN_DIR_PATTERNS[*]})"
    s+=", in_extensions=$(array_to_string ${IN_EXTENSIONS[*]})"
    s+=", in_file_patterns=$(array_to_string ${IN_FILE_PATTERNS[*]})"
    s+=", in_file_types=$(array_to_string ${IN_FILE_TYPES[*]})"
    s+=", max_depth=$MAX_DEPTH"
    s+=", max_last_mod=\"$MAX_LAST_MOD\""
    s+=", max_size=$MAX_SIZE"
    s+=", min_depth=$MIN_DEPTH"
    s+=", min_last_mod=\"$MIN_LAST_MOD\""
    s+=", min_size=$MIN_SIZE"
    s+=", out_archive_extensions=$(array_to_string ${OUT_ARCHIVE_EXTENSIONS[*]})"
    s+=", out_archive_file_patterns=$(array_to_string ${OUT_ARCHIVE_FILE_PATTERNS[*]})"
    s+=", out_dir_patterns=$(array_to_string ${OUT_DIR_PATTERNS[*]})"
    s+=", out_extensions=$(array_to_string ${OUT_EXTENSIONS[*]})"
    s+=", out_file_patterns=$(array_to_string ${OUT_FILE_PATTERNS[*]})"
    s+=", out_file_types=$(array_to_string ${OUT_FILE_TYPES[*]})"
    s+=", paths=$(array_to_string ${PATHS[*]})"
    s+=", print_dirs=$PRINT_DIRS"
    s+=", print_files=$PRINT_FILES"
    s+=", print_usage=$PRINT_USAGE"
    s+=", print_version=$PRINT_VERSION"
    s+=", recursive=$RECURSIVE"
    s+=", sort_by=$SORT_BY"
    s+=", sort_case_sensitive=$SORT_CASE_SENSITIVE"
    s+=", sort_descending=$SORT_DESCENDING"
    s+=", verbose=$VERBOSE"
    echo "$s"
}
