#!/bin/bash
################################################################################
#
# update.sh
#
# Runs checks projects for updates to language and dependencies.
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Load the generic update functions
source "$DIR/update_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: update.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Update Functions
########################################

update_xfind_version () {
    local lang_name="$1"
    local version_name="$2"

    # reset before calling
    UPDATE_LASTEXITCODE=0

    function_name="update_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XFIND_PATH" "$version_name"
    else
        log_error "update function not found: $function_name"
        UPDATE_LASTEXITCODE=1
    fi

    # log "UPDATE_LASTEXITCODE: $UPDATE_LASTEXITCODE"
    if [ "$UPDATE_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name update succeeded"
        SUCCESSFUL_UPDATES+=($version_name)
    else
        log_error "$version_name update failed"
        FAILED_UPDATES+=($version_name)
    fi
}

update_bashfind () {
    echo
    hdr "update_bashfind"

    update_xfind_version "bash" "bashfind"
}

update_cfind () {
    echo
    hdr "update_cfind"

    update_xfind_version "c" "cfind"
}

update_cljfind () {
    echo
    hdr "update_cljfind"

    update_xfind_version "clojure" "cljfind"
}

update_cppfind () {
    echo
    hdr "update_cppfind"

    update_xfind_version "cpp" "cppfind"
}

update_csfind () {
    echo
    hdr "update_csfind"

    update_xfind_version "csharp" "csfind"
}

update_dartfind () {
    echo
    hdr "update_dartfind"

    update_xfind_version "dart" "dartfind"
}

update_exfind () {
    echo
    hdr "update_exfind"

    update_xfind_version "elixir" "exfind"
}

update_fsfind () {
    echo
    hdr "update_fsfind"

    update_xfind_version "fsharp" "fsfind"
}

update_gofind () {
    echo
    hdr "update_gofind"

    update_xfind_version "go" "gofind"
}

update_groovyfind () {
    echo
    hdr "update_groovyfind"

    update_xfind_version "groovy" "groovyfind"
}

update_hsfind () {
    echo
    hdr "update_hsfind"

    update_xfind_version "haskell" "hsfind"
}

update_javafind () {
    echo
    hdr "update_javafind"

    update_xfind_version "java" "javafind"
}

update_jsfind () {
    echo
    hdr "update_jsfind"

    update_xfind_version "javascript" "jsfind"
}

update_ktfind () {
    echo
    hdr "update_ktfind"

    update_xfind_version "kotlin" "ktfind"
}

update_objcfind () {
    echo
    hdr "update_objcfind"

    update_xfind_version "objc" "objcfind"
}

# update_mlfind () {
#     echo
#     hdr "update_mlfind"
#
#     cd "$MLFIND_PATH"
#
#     log "Unit-testing mlfind"
#     ./unittest.sh
#
#     cd -
# }

update_plfind () {
    echo
    hdr "update_plfind"

    update_xfind_version "perl" "plfind"
}

update_phpfind () {
    echo
    hdr "update_phpfind"

    update_xfind_version "php" "phpfind"
}

update_ps1find () {
    echo
    hdr "update_ps1find"

    update_xfind_version "powershell" "ps1find"
}

update_pyfind () {
    echo
    hdr "update_pyfind"

    update_xfind_version "python" "pyfind"
}

update_rbfind () {
    echo
    hdr "update_rbfind"

    update_xfind_version "ruby" "rbfind"
}

update_rsfind () {
    echo
    hdr "update_rsfind"

    update_xfind_version "rust" "rsfind"
}

update_scalafind () {
    echo
    hdr "update_scalafind"

    update_xfind_version "scala" "scalafind"
}

update_swiftfind () {
    echo
    hdr "update_swiftfind"

    update_xfind_version "swift" "swiftfind"
}

update_tsfind () {
    echo
    hdr "update_tsfind"

    update_xfind_version "typescript" "tsfind"
}

update_all () {
    hdr "update_all"

    update_bashfind

    update_cfind

    update_cljfind

    update_cppfind

    update_csfind

    update_dartfind

    update_exfind

    update_fsfind

    update_gofind

    update_groovyfind

    update_hsfind

    update_javafind

    update_jsfind

    update_ktfind

    update_objcfind

    # update_mlfind

    update_plfind

    update_phpfind

    update_ps1find

    update_pyfind

    update_rbfind

    update_rsfind

    update_scalafind

    update_swiftfind

    update_tsfind
}


########################################
# Update main
########################################
echo
hdr "xfind update script"
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
UPDATE_ALL=
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
            UPDATE_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "UPDATE_ALL: $UPDATE_ALL"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$UPDATE_ALL" ]
then
    update_all
    print_update_results
    exit
fi

if [ ${#TARGET_LANGS[@]} == 0 ]
then
    usage
fi

for TARGET_LANG in ${TARGET_LANGS[*]}
do
    case $TARGET_LANG in
        bash)
            update_bashfind
            ;;
        c)
            update_cfind
            ;;
        clj | clojure)
            update_cljfind
            ;;
        cpp)
            update_cppfind
            ;;
        cs | csharp)
            update_csfind
            ;;
        dart)
            update_dartfind
            ;;
        elixir | ex)
            update_exfind
            ;;
        fs | fsharp)
            update_fsfind
            ;;
        go)
            update_gofind
            ;;
        groovy)
            update_groovyfind
            ;;
        haskell | hs)
            update_hsfind
            ;;
        java)
            update_javafind
            ;;
        javascript | js)
            update_jsfind
            ;;
        kotlin | kt)
            update_ktfind
            ;;
        objc)
            update_objcfind
            ;;
        # ocaml | ml)
        #     update_mlfind
        #     ;;
        perl | pl)
            update_plfind
            ;;
        php)
            update_phpfind
            ;;
        ps1 | powershell | pwsh)
            update_ps1find
            ;;
        py | python)
            update_pyfind
            ;;
        rb | ruby)
            update_rbfind
            ;;
        rs | rust)
            update_rsfind
            ;;
        scala)
            update_scalafind
            ;;
        swift)
            update_swiftfind
            ;;
        ts | typescript)
            update_tsfind
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_update_results
