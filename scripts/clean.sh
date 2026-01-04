#!/bin/bash
################################################################################
#
# clean.sh
#
# Clean xfind language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# source "$DIR/config.sh"
# source "$DIR/common.sh"

# Load the generic build functions
source "$DIR/clean_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: clean.sh [-h|--help] [--lock] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Clean Functions
########################################

clean_xfind_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="clean_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XFIND_PATH" "$version_name"
    else
        log_error "clean function not found: $function_name"
        CLEAN_LASTEXITCODE=1
    fi

    # log "CLEAN_LASTEXITCODE: $CLEAN_LASTEXITCODE"
    if [ "$CLEAN_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name clean succeeded"
        SUCCESSFUL_CLEANS+=($version_name)
    else
        log_error "$version_name clean failed"
        FAILED_CLEANS+=($version_name)
    fi
}

clean_bashfind () {
    echo
    hdr "clean_bashfind"
    log "Nothing to do for bash"
}

clean_cfind () {
    echo
    hdr "clean_cfind"

    clean_xfind_version "c" "cfind"
}

clean_cljfind () {
    echo
    hdr "clean_cljfind"

    clean_xfind_version "clojure" "cljfind"
}

clean_cppfind () {
    echo
    hdr "clean_cppfind"

    clean_xfind_version "cpp" "cppfind"
}

clean_csfind () {
    echo
    hdr "clean_csfind"

    clean_xfind_version "csharp" "csfind"
}

clean_dartfind () {
    echo
    hdr "clean_dartfind"

    clean_xfind_version "dart" "dartfind"
}

clean_exfind () {
    echo
    hdr "clean_exfind"

    clean_xfind_version "elixir" "exfind"
}

clean_fsfind () {
    echo
    hdr "clean_fsfind"

    clean_xfind_version "fsharp" "fsfind"
}

clean_gofind () {
    echo
    hdr "clean_gofind"

    clean_xfind_version "go" "gofind"
}

clean_groovyfind () {
    echo
    hdr "clean_groovyfind"

    clean_xfind_version "groovy" "groovyfind"
}

clean_hsfind () {
    echo
    hdr "clean_hsfind"

    clean_xfind_version "haskell" "hsfind"
}

clean_javafind () {
    echo
    hdr "clean_javafind"

    clean_xfind_version "java" "javafind"
}

clean_jsfind () {
    echo
    hdr "clean_jsfind"

    clean_xfind_version "javascript" "jsfind"
}

clean_ktfind () {
    echo
    hdr "clean_ktfind"

    clean_xfind_version "kotlin" "ktfind"
}

clean_mlfind () {
    echo
    hdr "clean_mlfind"

    clean_xfind_version "ocaml" "mlfind"
}

clean_objcfind () {
    echo
    hdr "clean_objcfind"

    clean_xfind_version "objc" "objcfind"
}

clean_plfind () {
    echo
    hdr "clean_plfind"

    clean_xfind_version "perl" "plfind"
}

clean_phpfind () {
    echo
    hdr "clean_phpfind"

    clean_xfind_version "php" "phpfind"
}

clean_ps1find () {
    echo
    hdr "clean_ps1find"

    clean_xfind_version "powershell" "ps1find"
}

clean_pyfind () {
    echo
    hdr "clean_pyfind"

    clean_xfind_version "python" "pyfind"
}

clean_rbfind () {
    echo
    hdr "clean_rbfind"

    clean_xfind_version "ruby" "rbfind"
}

clean_rsfind () {
    echo
    hdr "clean_rsfind"

    clean_xfind_version "rust" "rsfind"
}

clean_scalafind () {
    echo
    hdr "clean_scalafind"

    clean_xfind_version "scala" "scalafind"
}

clean_swiftfind () {
    echo
    hdr "clean_swiftfind"

    clean_xfind_version "swift" "swiftfind"
}

clean_tsfind () {
    echo
    hdr "clean_tsfind"

    clean_xfind_version "typescript" "tsfind"
}

clean_linux () {
    hdr "clean_linux"

    clean_bashfind

    clean_cfind

    # clean_cljfind

    # clean_cppfind

    clean_csfind

    # clean_dartfind

    clean_exfind

    clean_fsfind

    clean_gofind

    clean_groovyfind

    # clean_hsfind

    clean_javafind

    clean_jsfind

    clean_ktfind

    # clean_objcfind

    # clean_mlfind

    clean_phpfind

    clean_plfind

    clean_pyfind

    clean_rbfind

    clean_rsfind

    # clean_scalafind

    clean_swiftfind

    clean_tsfind
}

clean_all () {
    hdr "clean_all"

    clean_bashfind

    clean_cfind

    clean_cljfind

    clean_cppfind

    clean_csfind

    clean_dartfind

    clean_exfind

    clean_fsfind

    clean_gofind

    clean_groovyfind

    clean_hsfind

    clean_javafind

    clean_jsfind

    clean_ktfind

    clean_objcfind

    clean_mlfind

    clean_plfind

    clean_phpfind

    clean_ps1find

    clean_pyfind

    clean_rbfind

    clean_rsfind

    clean_scalafind

    clean_swiftfind

    clean_tsfind
}


########################################
# Clean Main
########################################
echo
hdr "xfind clean script"
log "user: $USER"
log "host: $HOSTNAME"
log "os: $(uname -o)"

# Get the current git branch and commit
# GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
GIT_BRANCH=$(git branch --show-current)
GIT_COMMIT=$(git rev-parse --short HEAD)
log "git branch: '$GIT_BRANCH' ($GIT_COMMIT)"

log "args: $*"

CLEAN_ALL=
HELP=
LOCKFILE=
TARGET_LANGS=()

if [ $# == 0 ]
then
    HELP=yes
fi

while [ -n "$1" ]
do
    case "$1" in
        --all | all)
            CLEAN_ALL=yes
            ;;
        -h | --help)
            HELP=yes
            ;;
        --lock)
            LOCKFILE=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "CLEAN_ALL: $CLEAN_ALL"
log "HELP: $HELP"
log "LOCKFILE: $LOCKFILE"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$CLEAN_ALL" ]
then
    clean_all
    print_clean_results
    exit
fi

if [ ${#TARGET_LANGS[@]} == 0 ]
then
    usage
fi

for TARGET_LANG in ${TARGET_LANGS[*]}
do
    case $TARGET_LANG in
        linux)
            clean_linux
            ;;
        bash)
            clean_bashfind
            ;;
        c)
            clean_cfind
            ;;
        clj | clojure)
            clean_cljfind
            ;;
        cpp)
            clean_cppfind
            ;;
        cs | csharp)
            clean_csfind
            ;;
        dart)
            clean_dartfind
            ;;
        elixir | ex)
            clean_exfind
            ;;
        fs | fsharp)
            clean_fsfind
            ;;
        go)
            clean_gofind
            ;;
        groovy)
            clean_groovyfind
            ;;
        haskell | hs)
            clean_hsfind
            ;;
        java)
            clean_javafind
            ;;
        javascript | js)
            clean_jsfind
            ;;
        kotlin | kt)
            clean_ktfind
            ;;
        objc)
            clean_objcfind
            ;;
        # ocaml | ml)
        #     clean_mlfind
        #     ;;
        perl | pl)
            clean_plfind
            ;;
        php)
            clean_phpfind
            ;;
        ps1 | powershell)
            clean_ps1find
            ;;
        py | python)
            clean_pyfind
            ;;
        rb | ruby)
            clean_rbfind
            ;;
        rs | rust)
            clean_rsfind
            ;;
        scala)
            clean_scalafind
            ;;
        swift)
            clean_swiftfind
            ;;
        ts | typescript)
            clean_tsfind
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_clean_results
