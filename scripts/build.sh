#!/bin/bash
################################################################################
#
# build.sh
#
# Build xfind language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Load the generic build functions
source "$DIR/build_functions.sh"


########################################
# Common Functions
########################################

usage () {
    echo -e "\nUsage: build.sh [-h|--help] [--debug] [--release] [--venv] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Build Functions
########################################

build_xfind_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="build_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XFIND_PATH" "$version_name"
    else
        log_error "build function not found: $function_name"
        BUILD_LASTEXITCODE=1
    fi

    # log "BUILD_LASTEXITCODE: $BUILD_LASTEXITCODE"
    if [ "$BUILD_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name build succeeded"
        SUCCESSFUL_BUILDS+=($version_name)
    else
        log_error "$version_name build failed"
        FAILED_BUILDS+=($version_name)
    fi
}

build_bashfind () {
    echo
    hdr "build_bashfind"

    build_xfind_version "bash" "bashfind"
}

build_cfind () {
    echo
    hdr "build_cfind"

    build_xfind_version "c" "cfind"
}

build_cljfind () {
    echo
    hdr "build_cljfind"

    build_xfind_version "clojure" "cljfind"
}

build_cppfind () {
    echo
    hdr "build_cppfind"

    build_xfind_version "cpp" "cppfind"
}

build_csfind () {
    echo
    hdr "build_csfind"

    build_xfind_version "csharp" "csfind"
}

build_dartfind () {
    echo
    hdr "build_dartfind"

    build_xfind_version "dart" "dartfind"
}

build_exfind () {
    echo
    hdr "build_exfind"

    build_xfind_version "elixir" "exfind"
}

build_fsfind () {
    echo
    hdr "build_fsfind"

    build_xfind_version "fsharp" "fsfind"
}

build_gofind () {
    echo
    hdr "build_gofind"

    build_xfind_version "go" "gofind"
}

build_groovyfind () {
    echo
    hdr "build_groovyfind"

    build_xfind_version "groovy" "groovyfind"
}

build_hsfind () {
    echo
    hdr "build_hsfind"

    build_xfind_version "haskell" "hsfind"
}

build_javafind () {
    echo
    hdr "build_javafind"

    build_xfind_version "java" "javafind"
}

build_jsfind () {
    echo
    hdr "build_jsfind"

    build_xfind_version "javascript" "jsfind"
}

build_ktfind () {
    echo
    hdr "build_ktfind"

    build_xfind_version "kotlin" "ktfind"
}

build_mlfind () {
    echo
    hdr "build_mlfind"

    build_xfind_version "ocaml" "mlfind"
}

build_objcfind () {
    echo
    hdr "build_objcfind"

    build_xfind_version "objc" "objcfind"
}

build_plfind () {
    echo
    hdr "build_plfind"

    build_xfind_version "perl" "plfind"
}

build_phpfind () {
    echo
    hdr "build_phpfind"

    build_xfind_version "php" "phpfind"
}

build_ps1find () {
    echo
    hdr "build_ps1find"

    build_xfind_version "powershell" "ps1find"
}

build_pyfind () {
    echo
    hdr "build_pyfind"

    build_xfind_version "python" "pyfind"
}

build_rbfind () {
    echo
    hdr "build_rbfind"

    build_xfind_version "ruby" "rbfind"
}

build_rsfind () {
    echo
    hdr "build_rsfind"

    build_xfind_version "rust" "rsfind"
}

build_scalafind () {
    echo
    hdr "build_scalafind"

    build_xfind_version "scala" "scalafind"
}

build_swiftfind () {
    echo
    hdr "build_swiftfind"

    build_xfind_version "swift" "swiftfind"
}

build_tsfind () {
    echo
    hdr "build_tsfind"

    build_xfind_version "typescript" "tsfind"
}

# build_linux - builds the versions that are currently supported in the linux container
# Notes about some of the builds:
# - build_cljfind   - this build is _really_ slow (10+ minutes?), so call its build directly if you want to try it
# - build_cppfind   - this build takes a decent amount of time to complete (though nowhere near as much as clojure)
# - build_gofind    - go is known for having very fast builds, and it's true, the only builds that are faster here
#                     are the ones that do nothing except copy over resources files (e.g. perl)
# - build_hsfind    - having some dependency issues that need to work through to get it buildling again
# - build_jsfind    - this fails to build in the vscode terminal right now due to some debug plugin issue; building
#                     in an external terminal fixes the problem
# - build_ktfind    - This build can sometimes be quite slow, other times fairly fast. In particular, the first
#                     time will likely be quite slow, and I think it will also be slow when a build hasn't been run
#                     in a while
# - build_objcfind  - not sure if it's even possible to build this on linux, but excluding for now
# - build_mlfind    - had a number of different issues trying to get this version building again, finally
#                     gave up for now after it appeared that there were a lot of changes to the main API, etc.
# - build_rsfind      - the first time this build is run it will pretty time-consuming, particularly for release
#                     target, but intermittent builds should be pretty fast
# - build_scalafind - this build isn't as slow as the clojure version's, but it's slow enough to run separately
# - build_tsfind    - this build has the same problem as build_jsfind; run the build in an external terminal
build_linux () {
    hdr "build_linux"

    time build_bashfind

    time build_cfind

    # time build_cljfind

    # time build_cppfind

    time build_csfind

    time build_dartfind

    time build_fsfind

    time build_gofind

    # time build_groovyfind

    time build_javafind

    time build_jsfind

    # time build_ktfind

    time build_plfind

    time build_phpfind

    time build_pyfind

    time build_rbfind

    time build_rsfind

    # time build_scalafind

    time build_swiftfind

    time build_tsfind
}

build_all () {
    hdr "build_all"

    time build_bashfind

    time build_cfind

    time build_cljfind

    time build_cppfind

    time build_csfind

    time build_dartfind

    time build_exfind

    time build_fsfind

    time build_gofind

    time build_groovyfind

    time build_hsfind

    time build_javafind

    time build_jsfind

    time build_ktfind

    time build_objcfind

    # time build_mlfind

    time build_plfind

    time build_phpfind

    time build_ps1find

    time build_pyfind

    time build_rbfind

    time build_rsfind

    time build_scalafind

    time build_swiftfind

    time build_tsfind
}


########################################
# Build Main
########################################
echo
hdr "xfind build script"
log "user: $USER"
log "host: $HOSTNAME"
log "os: $(uname -o)"

# Get the current git branch and commit
GIT_BRANCH=$(git branch --show-current)
GIT_COMMIT=$(git rev-parse --short HEAD)
log "git branch: '$GIT_BRANCH' ($GIT_COMMIT)"

log "args: $*"

HELP=
DEBUG=
RELEASE=
VENV=
BUILD_ALL=
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
        --debug)
            DEBUG=yes
            ;;
        --release)
            RELEASE=yes
            ;;
        --venv)
            VENV=yes
            ;;
        --all | all)
            BUILD_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

if [ -z "$RELEASE" ]
then
    DEBUG=yes
fi

# log the settings
log "HELP: $HELP"
log "DEBUG: $DEBUG"
log "RELEASE: $RELEASE"
log "VENV: $VENV"
log "BUILD_ALL: $BUILD_ALL"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$BUILD_ALL" ]
then
    build_all
    print_build_results
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
            build_linux
            ;;
        bash)
            time build_bashfind
            ;;
        c)
            time build_cfind
            ;;
        clj | clojure)
            time build_cljfind
            ;;
        cpp)
            time build_cppfind
            ;;
        cs | csharp)
            time build_csfind
            ;;
        dart)
            time build_dartfind
            ;;
        ex | elixir)
            time build_exfind
            ;;
        fs | fsharp)
            time build_fsfind
            ;;
        go)
            time build_gofind
            ;;
        groovy)
            time build_groovyfind
            ;;
        haskell | hs)
            time build_hsfind
            ;;
        java)
            time build_javafind
            ;;
        javascript | js)
            time build_jsfind
            ;;
        kotlin | kt)
            time build_ktfind
            ;;
        objc)
            time build_objcfind
            ;;
        # ocaml | ml)
        #     time build_mlfind
        #     ;;
        perl | pl)
            time build_plfind
            ;;
        php)
            time build_phpfind
            ;;
        ps1 | powershell | pwsh)
            time build_ps1find
            ;;
        py | python)
            time build_pyfind
            ;;
        rb | ruby)
            time build_rbfind
            ;;
        rs | rust)
            time build_rsfind
            ;;
        scala)
            time build_scalafind
            ;;
        swift)
            time build_swiftfind
            ;;
        ts | typescript)
            time build_tsfind
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_build_results
