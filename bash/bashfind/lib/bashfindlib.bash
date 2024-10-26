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
DEBUG=false
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

FILE_RESULTS=()


exit_with_error () {
    local error="$1"
    local color="$RED"
    # TODO: add COLORIZE option
    # echo -e "\n${color}ERROR: $error${COLOR_RESET}"
    echo -e "\nERROR: $error"
    usage
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
            exit_with_error "Startpath not found"
        fi
        if [ ! -r $p ]
        then
            exit_with_error "Startpath not readable"
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

is_matching_dir () {
    local dir_path="$1"

    if [ "$dir_path" == "" -o "$dir_path" == "." -o "$dir_path" == ".." ]
    then
        return 1
    fi

    if [ $INCLUDE_HIDDEN == false ]
    then
        local base_dir=$(basename $dir_path)
        if [[ "${base_dir:0:1}" == "." ]]
        then
            return 0
        fi
    fi

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

    if [ ${#OUT_DIR_PATTERNS[@]} -gt 0 ]
    then
        for p in ${OUT_DIR_PATTERNS[*]}
        do
            if [[ "$dir_path" =~ $p ]]
            then
                return 0
            fi
        done
        return 1
    fi

    return 1
}

is_matching_archive_file () {
    local file_path="$1"
    local file_name=$(basename $file_path)

    if [ $INCLUDE_HIDDEN == false ]
    then
        if [[ "${file_name:0:1}" == "." ]]
        then
            return 0
        fi
    fi

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

is_matching_file () {
    local file_path="$1"
    local file_type="$2"
    local file_size=$3
    local last_mod=$4
    local file_name=$(basename $file_path)

    if [ $INCLUDE_HIDDEN == false ]
    then
        if [[ "${file_name:0:1}" == "." ]]
        then
            return 0
        fi
    fi

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

rec_find_path () {
    local path="$1"
    local current_depth=$2
    local recurse=true

    if [ $current_depth == $MAX_DEPTH ]
    then
        recurse=false
    fi

    if [ $MAX_DEPTH -gt -1 -a $current_depth -gt $MAX_DEPTH ]
    then
        return 0
    fi

    local path_dirs=()
    local path_files=()
    if [ "$recurse" == true ]
    then
        path_dirs=$(find $path -maxdepth 1 -type d | grep -v "^.$" | grep -v "^..$" | grep -v "^$path$")
    fi
    if [ $MIN_DEPTH -lt 0 -o $current_depth -ge $MIN_DEPTH ]
    then
        path_files=$(find $path -maxdepth 1 -type f)
    fi

    for d in ${path_dirs[*]}
    do
        is_matching_dir "$d"
        local dir_match=$?
        if [ "$dir_match" == 1 ]
        then
            rec_find_path $d $(($current_depth + 1))
        fi
    done

    for f in ${path_files[*]}
    do
        local file_name=$(basename $f)
        local file_type='unknown'
        local file_size=0
        local last_mod=0

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
                    local dir_path=$(dirname $f)
                    FILE_RESULTS+=("$dir_path,$file_name,$file_type,$file_size,$last_mod")
                fi
            fi
        else
            if [ $NEED_FILE_TYPE == true ]
            then
                file_type=$(get_file_type $f)
            fi
            if [ $NEED_FILE_SIZE == true ]
            then
                file_size=$(stat -f %z $f)
            fi
            if [ $NEED_LAST_MOD == true ]
            then
                last_mod=$(stat -f %m $f)
            fi
            is_matching_file "$file_name" "$file_type" $file_size $last_mod
            if [ $? == 1 ]
            then
                local dir_path=$(dirname $f)
                FILE_RESULTS+=("$dir_path,$file_name,$file_type,$file_size,$last_mod")
            fi
        fi
    done
}

find_path () {
    local path="$1"

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
        is_matching_dir "$path"
        local dir_match=$?
        if [ "$dir_match" == 1 ]
        then
            rec_find_path $path 1
        fi
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

get_option_desc () {
    local long=$1
    local desc=$(jq -r ".findoptions[] | select(.long == \"$long\") | .desc" $FIND_OPTIONS_PATH)
    echo $desc
}

usage () {
    # TODO: load options from file
    s="\nUsage:\n bashfind [options] <path> [<path> ...]\n\nOptions:"
    s+="\n --archivesonly            $(get_option_desc 'archivesonly')"
    s+="\n -d,--in-dirpattern        $(get_option_desc 'in-dirpattern')"
    s+="\n -D,--out-dirpattern       $(get_option_desc 'out-dirpattern')"
    s+="\n --debug                   $(get_option_desc 'debug')"
    s+="\n --excludehidden           $(get_option_desc 'excludehidden')"
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
    s+="\n --noprintdirs             $(get_option_desc 'noprintdirs')"
    s+="\n --noprintfiles            $(get_option_desc 'noprintfiles')"
    s+="\n --out-archiveext          $(get_option_desc 'out-archiveext')"
    s+="\n --out-archivefilepattern  $(get_option_desc 'out-archivefilepattern')"
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
            -h | --help)
                PRINT_USAGE=true
                ;;
            --includearchives)
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
                settings_from_file $arg2
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
            --verbose)
                VERBOSE=true
                ;;
            -V | --version)
                PRINT_VERSION=true
                ;;
            *)
                if [[ "${arg:0:1}" == "-" ]]
                then
                    exit_with_error "Invalid option: $arg"
                fi
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

settings_from_json () {
    local json="$1"
    local array_keys=( $(echo "$json" | jq -r 'with_entries(select(.value | type == "array")) | keys | .[]') )
    for k in ${array_keys[*]}
    do
        local arr=( $(echo "$json" | jq -r ".\"$k\" | .[]") )
        args=()
        for a in ${arr[*]}
        do
            args+=("--$k")
            args+=("$a")
        done
        settings_from_args ${args[*]}
    done
    local other_keys=( $(echo "$json" | jq -r 'with_entries(select(.value | type != "array")) | keys | .[]') )
    other_args=()
    for k in ${other_keys[*]}
    do
        local v=$(echo "$json" | jq -r ".$k")
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
            other_args+=("--$k")
            other_args+=("$v")
        fi
    done
    settings_from_args ${other_args[*]}
}

settings_from_file () {
    local file_path="$1"
    local file_name=$(basename $file_path)
    local file_ext="${file_name##*.}"

    if [ -f "$file_path" -a -r "$file_path" -a "$file_ext" == "json" ]
    then
        json=$(cat $file_path)
        settings_from_json "$json"
    else
        exit_with_error "Invalid settings file: $file_path"
    fi
}

settings_to_string () {
    s="archives_only=$ARCHIVES_ONLY"
    s+=", debug=$DEBUG"
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
