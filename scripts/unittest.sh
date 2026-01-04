#!/bin/bash
################################################################################
#
# unittest.sh
#
# Run unit tests of xfind language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# Load the generic unittest functions
source "$DIR/unittest_functions.sh"


########################################
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: unittest.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Unit Test Functions
########################################

unittest_xfind_version () {
    local lang_name="$1"
    local version_name="$2"

    function_name="unittest_${lang_name}_version"
    # log "function_name: $function_name"

    if [[ "$(type -t $function_name)" == "function" ]]
    then
        "$function_name" "$XFIND_PATH" "$version_name"
    else
        log_error "unittest function not found: $function_name"
        UNITTEST_LASTEXITCODE=1
    fi

    # log "UNITTEST_LASTEXITCODE: $UNITTEST_LASTEXITCODE"
    if [ "$UNITTEST_LASTEXITCODE" -eq 0 ]
    then
        log "$version_name tests succeeded"
        SUCCESSFUL_TESTS+=($version_name)
    else
        log_error "$version_name tests failed"
        FAILED_TESTS+=($version_name)
    fi
}

unittest_bashfind () {
    echo
    hdr "unittest_bashfind"

    unittest_xfind_version "bash" "bashfind"
}

unittest_cfind () {
    echo
    hdr "unittest_cfind"

    unittest_xfind_version "c" "cfind"
}

unittest_cljfind () {
    echo
    hdr "unittest_cljfind"

    unittest_xfind_version "clojure" "cljfind"
}

unittest_cppfind () {
    echo
    hdr "unittest_cppfind"

    unittest_xfind_version "cpp" "cppfind"
}

unittest_csfind () {
    echo
    hdr "unittest_csfind"

    unittest_xfind_version "csharp" "csfind"
}

unittest_dartfind () {
    echo
    hdr "unittest_dartfind"

    unittest_xfind_version "dart" "dartfind"
}

unittest_exfind () {
    echo
    hdr "unittest_exfind"

    unittest_xfind_version "elixir" "exfind"
}

unittest_fsfind () {
    echo
    hdr "unittest_fsfind"

    unittest_xfind_version "fsharp" "fsfind"
}

unittest_gofind () {
    echo
    hdr "unittest_gofind"

    unittest_xfind_version "go" "gofind"
}

unittest_groovyfind () {
    echo
    hdr "unittest_groovyfind"

    unittest_xfind_version "groovy" "groovyfind"
}

unittest_hsfind () {
    echo
    hdr "unittest_hsfind"

    unittest_xfind_version "haskell" "hsfind"
}

unittest_javafind () {
    echo
    hdr "unittest_javafind"

    unittest_xfind_version "java" "javafind"
}

unittest_jsfind () {
    echo
    hdr "unittest_jsfind"

    unittest_xfind_version "javascript" "jsfind"
}

unittest_ktfind () {
    echo
    hdr "unittest_ktfind"

    unittest_xfind_version "kotlin" "ktfind"
}

unittest_mlfind () {
    echo
    hdr "unittest_mlfind"

    unittest_xfind_version "ocaml" "mlfind"
}

unittest_objcfind () {
    echo
    hdr "unittest_objcfind"

    unittest_xfind_version "objc" "objcfind"
}

unittest_plfind () {
    echo
    hdr "unittest_plfind"

    unittest_xfind_version "perl" "plfind"
}

unittest_phpfind () {
    echo
    hdr "unittest_phpfind"

    unittest_xfind_version "php" "phpfind"
}

unittest_ps1find () {
    echo
    hdr "unittest_ps1find"

    unittest_xfind_version "powershell" "ps1find"
}

unittest_pyfind () {
    echo
    hdr "unittest_pyfind"

    unittest_xfind_version "python" "pyfind"
}

unittest_rbfind () {
    echo
    hdr "unittest_rbfind"

    unittest_xfind_version "ruby" "rbfind"
}

unittest_rsfind () {
    echo
    hdr "unittest_rsfind"

    unittest_xfind_version "rust" "rsfind"
}

unittest_scalafind () {
    echo
    hdr "unittest_scalafind"

    unittest_xfind_version "scala" "scalafind"
}

unittest_swiftfind () {
    echo
    hdr "unittest_swiftfind"

    unittest_xfind_version "swift" "swiftfind"
}

unittest_tsfind () {
    echo
    hdr "unittest_tsfind"

    unittest_xfind_version "typescript" "tsfind"
}

# unittest_linux - builds the versions that are currently supported in the linux container
# Notes about some of the builds:
# - unittest_cljfind   - this build is _really_ slow (10+ minutes?), so call its build directly if you want to try it
# - unittest_cppfind   - this build takes a decent amount of time to complete (though nowhere near as much as clojure)
# - unittest_gofind    - go is known for having very fast builds, and it's true, the only builds that are faster here
#                     are the ones that do nothing except copy over resources files (e.g. perl)
# - unittest_hsfind    - having some dependency issues that need to work through to get it buildling again
# - unittest_jsfind    - this fails to build in the vscode terminal right now due to some debug plugin issue; building
#                     in an external terminal fixes the problem
# - unittest_ktfind    - This build can sometimes be quite slow, other times fairly fast. In particular, the first
#                     time will likely be quite slow, and I think it will also be slow when a build hasn't been run
#                     in a while
# - unittest_objcfind  - not sure if it's even possible to build this on linux, but excluding for now
# - unittest_mlfind    - had a number of different issues trying to get this version building again, finally
#                     gave up for now after it appeared that there were a lot of changes to the main API, etc.
# - unittest_rsfind      - the first time this build is run it will pretty time-consuming, particularly for release
#                     target, but intermittent builds should be pretty fast
# - unittest_scalafind - this build isn't as slow as the clojure version's, but it's slow enough to run separately
# - unittest_tsfind    - this build has the same problem as unittest_jsfind; run the build in an external terminal
unittest_linux () {
    hdr "unittest_linux"

    time unittest_bashfind

    time unittest_cfind

    # time unittest_cljfind

    # time unittest_cppfind

    time unittest_csfind

    time unittest_dartfind

    time unittest_fsfind

    time unittest_gofind

    # time unittest_groovyfind

    time unittest_javafind

    time unittest_jsfind

    # time unittest_ktfind

    time unittest_plfind

    time unittest_phpfind

    time unittest_pyfind

    time unittest_rbfind

    time unittest_rsfind

    # time unittest_scalafind

    time unittest_swiftfind

    time unittest_tsfind
}

unittest_all () {
    hdr "unittest_all"

    time unittest_bashfind

    time unittest_cfind

    time unittest_cljfind

    time unittest_cppfind

    time unittest_csfind

    time unittest_dartfind

    time unittest_exfind

    time unittest_fsfind

    time unittest_gofind

    time unittest_groovyfind

    time unittest_hsfind

    time unittest_javafind

    time unittest_jsfind

    time unittest_ktfind

    time unittest_objcfind

    # time unittest_mlfind

    time unittest_plfind

    time unittest_phpfind

    time unittest_ps1find

    time unittest_pyfind

    time unittest_rbfind

    time unittest_rsfind

    time unittest_scalafind

    time unittest_swiftfind

    time unittest_tsfind
}


########################################
# Unit Test Main
########################################
echo
hdr "xfind unittest script"
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
    unittest_all
    print_test_results
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
            unittest_linux
            ;;
        bash)
            time unittest_bashfind
            ;;
        c)
            time unittest_cfind
            ;;
        clj | clojure)
            time unittest_cljfind
            ;;
        cpp)
            time unittest_cppfind
            ;;
        cs | csharp)
            time unittest_csfind
            ;;
        dart)
            time unittest_dartfind
            ;;
        ex | elixir)
            time unittest_exfind
            ;;
        fs | fsharp)
            time unittest_fsfind
            ;;
        go)
            time unittest_gofind
            ;;
        groovy)
            time unittest_groovyfind
            ;;
        haskell | hs)
            time unittest_hsfind
            ;;
        java)
            time unittest_javafind
            ;;
        javascript | js)
            time unittest_jsfind
            ;;
        kotlin | kt)
            time unittest_ktfind
            ;;
        objc)
            time unittest_objcfind
            ;;
        # ocaml | ml)
        #     time unittest_mlfind
        #     ;;
        perl | pl)
            time unittest_plfind
            ;;
        php)
            time unittest_phpfind
            ;;
        ps1 | powershell | pwsh)
            time unittest_ps1find
            ;;
        py | python)
            time unittest_pyfind
            ;;
        rb | ruby)
            time unittest_rbfind
            ;;
        rs | rust)
            time unittest_rsfind
            ;;
        scala)
            time unittest_scalafind
            ;;
        swift)
            time unittest_swiftfind
            ;;
        ts | typescript)
            time unittest_tsfind
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_test_results
