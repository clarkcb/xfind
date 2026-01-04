#!/bin/bash
################################################################################
#
# lint.sh
#
# Run static code analysis tools on xfind language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Load the generic lint functions
source "$DIR/lint_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: lint.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Lint Functions
########################################

lint_xfind_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="lint_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XFIND_PATH" "$version_name"
    else
        log_error "lint function not found: $function_name"
        LINT_LASTEXITCODE=1
    fi

    # log "LINT_LASTEXITCODE: $LINT_LASTEXITCODE"
    if [ "$LINT_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name lint succeeded"
        SUCCESSFUL_LINTS+=($version_name)
    else
        log_error "$version_name lint failed"
        FAILED_LINTS+=($version_name)
    fi
}

lint_bashfind () {
    echo
    hdr "lint_bashfind"

    log "not implemented at this time"
}

lint_cfind () {
    echo
    hdr "lint_cfind"

    log "not implemented at this time"
}

lint_cljfind () {
    echo
    hdr "lint_cljfind"

    lint_xfind_version "clojure" "cljfind"
}

lint_cppfind () {
    echo
    hdr "lint_cppfind"

    log "not implemented at this time"
}

lint_csfind () {
    echo
    hdr "lint_csfind"

    log "not implemented at this time"
}

lint_dartfind () {
    echo
    hdr "lint_dartfind"

    lint_xfind_version "dart" "dartfind"
}

lint_exfind () {
    echo
    hdr "lint_exfind"

    lint_xfind_version "elixir" "exfind"
}

lint_fsfind () {
    echo
    hdr "lint_fsfind"

    log "not implemented at this time"
}

lint_gofind () {
    echo
    hdr "lint_gofind"

    lint_xfind_version "go" "gofind"
}

lint_groovyfind () {
    echo
    hdr "lint_groovyfind"

    lint_xfind_version "groovy" "groovyfind"
}

lint_hsfind () {
    echo
    hdr "lint_hsfind"

    lint_xfind_version "haskell" "hsfind"
}

lint_javafind () {
    echo
    hdr "lint_javafind"

    lint_xfind_version "java" "javafind"
}

lint_jsfind () {
    echo
    hdr "lint_jsfind"

    lint_xfind_version "javascript" "jsfind"
}

lint_ktfind () {
    echo
    hdr "lint_ktfind"

    lint_xfind_version "kotlin" "ktfind"
}

lint_mlfind () {
    echo
    hdr "lint_mlfind"

    # TODO: probably want to delete the _build directory
}

lint_objcfind () {
    echo
    hdr "lint_objcfind"

    log "not implemented at this time"
}

lint_phpfind () {
    echo
    hdr "lint_phpfind"

    lint_xfind_version "php" "phpfind"
}

lint_plfind () {
    echo
    hdr "lint_plfind"

    log "not implemented at this time"
}

lint_ps1find () {
    echo
    hdr "lint_ps1find"
    log "Nothing to do for powershell"
    # TODO: do we want to uninstall?
}

lint_pyfind () {
    echo
    hdr "lint_pyfind"

    lint_xfind_version "python" "pyfind"
}

lint_rbfind () {
    echo
    hdr "lint_rbfind"

    lint_xfind_version "ruby" "rbfind"
}

lint_rsfind () {
    echo
    hdr "lint_rsfind"

    log "not implemented at this time"
}

lint_scalafind () {
    echo
    hdr "lint_scalafind"

    lint_xfind_version "scala" "scalafind"
}

lint_swiftfind () {
    echo
    hdr "lint_swiftfind"

    lint_xfind_version "swift" "swiftfind"
}

lint_tsfind () {
    echo
    hdr "lint_tsfind"

    log "not implemented at this time"
}

lint_linux () {
    hdr "lint_linux"

    lint_bashfind

    lint_cfind

    # lint_cljfind

    # lint_cppfind

    lint_csfind

    # lint_dartfind

    lint_exfind

    lint_fsfind

    lint_gofind

    lint_groovyfind

    # lint_hsfind

    lint_javafind

    lint_jsfind

    lint_ktfind

    # lint_objcfind

    # lint_mlfind

    lint_phpfind

    lint_plfind

    lint_pyfind

    lint_rbfind

    lint_rsfind

    # lint_scalafind

    lint_swiftfind

    lint_tsfind
}

lint_all () {
    hdr "lint_all"

    lint_bashfind

    lint_cfind

    lint_cljfind

    lint_cppfind

    lint_csfind

    lint_dartfind

    lint_exfind

    lint_fsfind

    lint_gofind

    lint_groovyfind

    lint_hsfind

    lint_javafind

    lint_jsfind

    lint_ktfind

    lint_objcfind

    # lint_mlfind

    lint_plfind

    lint_phpfind

    lint_ps1find

    lint_pyfind

    lint_rbfind

    lint_rsfind

    lint_scalafind

    lint_swiftfind

    lint_tsfind
}


########################################
# Lint Main
########################################
echo
hdr "xfind lint script"
log "user: $USER"
log "host: $HOSTNAME"
log "os: $(uname -o)"

# Get the current git branch and commit
# GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
GIT_BRANCH=$(git branch --show-current)
GIT_COMMIT=$(git rev-parse --short HEAD)
log "git branch: '$GIT_BRANCH' ($GIT_COMMIT)"

log "args: $*"

HELP=
LINT_ALL=
TARGET_LANGS=()

if [ $# == 0 ]
then
    HELP=yes
fi

while [ -n "$1" ]
do
    case "$1" in
        -h | --help)
            HELP=yes
            ;;
        --all | all)
            LINT_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "LINT_ALL: $LINT_ALL"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$LINT_ALL" ]
then
    lint_all
    print_lint_results
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
            lint_linux
            ;;
        bash)
            lint_bashfind
            ;;
        c)
            lint_cfind
            ;;
        clj | clojure)
            lint_cljfind
            ;;
        cpp)
            lint_cppfind
            ;;
        cs | csharp)
            lint_csfind
            ;;
        dart)
            lint_dartfind
            ;;
        elixir | ex)
            lint_exfind
            ;;
        fs | fsharp)
            lint_fsfind
            ;;
        go)
            lint_gofind
            ;;
        groovy)
            lint_groovyfind
            ;;
        haskell | hs)
            lint_hsfind
            ;;
        java)
            lint_javafind
            ;;
        javascript | js)
            lint_jsfind
            ;;
        kotlin | kt)
            lint_ktfind
            ;;
        objc)
            lint_objcfind
            ;;
        # ocaml | ml)
        #     lint_mlfind
        #     ;;
        perl | pl)
            lint_plfind
            ;;
        php)
            lint_phpfind
            ;;
        ps1 | powershell)
            lint_ps1find
            ;;
        py | python)
            lint_pyfind
            ;;
        rb | ruby)
            lint_rbfind
            ;;
        rs | rust)
            lint_rsfind
            ;;
        scala)
            lint_scalafind
            ;;
        swift)
            lint_swiftfind
            ;;
        ts | typescript)
            lint_tsfind
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_lint_results