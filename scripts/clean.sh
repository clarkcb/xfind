#!/bin/bash
################################################################################
#
# clean.sh
#
# Runs a clean (remove generated files) for each language version
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"

# Add failed builds to this array and report failed builds at the end
FAILED_BUILDS=()


########################################
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: clean.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}

# clean_json_resources
clean_json_resources () {
    local resources_path="$1"
    for f in $(find "$resources_path" -name "*.json" -type f -maxdepth 1)
    do
        log "rm $f"
        rm "$f"
    done
}

# clean_test_resources
clean_test_resources () {
    local resources_path="$1"
    for f in $(find "$resources_path" -name "testFile*.txt" -type f -maxdepth 1)
    do
        log "rm $f"
        rm "$f"
    done
}

print_failed_builds () {
    if [ ${#FAILED_BUILDS[@]} -gt 0 ]
    then
        log_error "Failed cleans: ${FAILED_BUILDS[*]}"
    else
        log "All cleans succeeded"
    fi
}


########################################
# Clean Functions
########################################

clean_bashfind () {
    echo
    hdr "clean_bashfind"
    log "Nothing to do for bash"
}

clean_cfind () {
    echo
    hdr "clean_cfind"

    cd "$CFIND_PATH"

    for c in $(find . -name "cmake-build-*" -type d -maxdepth 1)
    do
        log "rm -rf $c"
        rm -rf "$c"
    done

    cd -
}

clean_cljfind () {
    echo
    hdr "clean_cljfind"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        log_error "You need to install lein"
        FAILED_BUILDS+=("cljfind")
        return
    fi

    cd "$CLJFIND_PATH"

    log "lein clean"
    lein clean

    clean_json_resources "$CLJFIND_PATH/resources"

    cd -
}

clean_cppfind () {
    echo
    hdr "clean_cppfind"

    cd "$CPPFIND_PATH"

    for c in $(find . -name "cmake-build-*" -type d -maxdepth 1)
    do
        log "rm -rf $c"
        rm -rf "$c"
    done

    cd -
}

clean_csfind () {
    echo
    hdr "clean_csfind"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("csfind")
        return
    fi

    cd "$CSFIND_PATH"

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    log "dotnet clean -v minimal"
    dotnet clean -v minimal

    for p in $(find "$CSFIND_PATH" -name "CsFind*" -type d -maxdepth 1)
    do
        if [ -d "$p" ]
        then
            log "rm -rf $p/bin"
            rm -rf "$p/bin"

            log "rm -rf $p/obj"
            rm -rf "$p/obj"
        fi
    done

    clean_json_resources "$CSFIND_PATH/CsFindLib/Resources"

    clean_test_resources "$CSFIND_PATH/CsFindTests/Resources"

    cd -
}

clean_dartfind () {
    echo
    hdr "clean_dartfind"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        FAILED_BUILDS+=("dartfind")
        return
    fi

    # pub cache repair is apparently the closest thing to clean for dart
    cd "$DARTFIND_PATH"

    log "dart pub cache repair"
    dart pub cache repair

    cd -
}

clean_exfind () {
    echo
    hdr "clean_exfind"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        FAILED_BUILDS+=("exfind")
        return
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        FAILED_BUILDS+=("exfind")
        return
    fi

    cd "$EXFIND_PATH"

    log "mix clean"
    mix clean

    cd -
}

clean_fsfind () {
    echo
    hdr "clean_fsfind"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("fsfind")
        return
    fi

    cd "$FSFIND_PATH"

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    log "dotnet clean -v minimal"
    dotnet clean -v minimal

    for p in $(find "$FSFIND_PATH" -name "FsFind*" -type d -maxdepth 1)
    do
        if [ -d "$p" ]
        then
            log "rm -rf $p/bin"
            rm -rf "$p/bin"

            log "rm -rf $p/obj"
            rm -rf "$p/obj"
        fi
    done

    clean_json_resources "$FSFIND_PATH/FsFindLib/Resources"

    clean_test_resources "$FSFIND_PATH/FsFindTests/Resources"

    cd -
}

clean_gofind () {
    echo
    hdr "clean_gofind"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        FAILED_BUILDS+=("gofind")
        return
    fi

    cd "$GOFIND_PATH"

    log "go clean"
    go clean

    cd -
}

clean_groovyfind () {
    echo
    hdr "clean_groovyfind"

    cd "$GROOVYFIND_PATH"

    GRADLE=
    # check for gradle wrapper
    if [ -f "gradlew" ]
    then
        GRADLE="./gradlew"
    elif [ -n "$(which gradle)" ]
    then
        GRADLE="gradle"
    else
        log_error "You need to install gradle"
        FAILED_BUILDS+=("groovyfind")
        return
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    clean_json_resources "$GROOVYFIND_PATH/src/main/resources"

    clean_test_resources "$GROOVYFIND_PATH/src/test/resources"

    cd -
}

clean_hsfind () {
    echo
    hdr "clean_hsfind"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        FAILED_BUILDS+=("hsfind")
        return
    fi

    cd "$HSFIND_PATH"

    log "stack clean"
    stack clean

    clean_json_resources "$HSFIND_PATH/data"

    cd -
}

clean_javafind () {
    echo
    hdr "clean_javafind"

    cd "$JAVAFIND_PATH"

    GRADLE=
    # check for gradle wrapper
    if [ -f "gradlew" ]
    then
        GRADLE="./gradlew"
    elif [ -n "$(which gradle)" ]
    then
        GRADLE="gradle"
    else
        log_error "You need to install gradle"
        FAILED_BUILDS+=("javafind")
        cd -
        return
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    clean_json_resources "$JAVAFIND_PATH/src/main/resources"

    clean_test_resources "$JAVAFIND_PATH/src/test/resources"

    cd -
}

clean_jsfind () {
    echo
    hdr "clean_jsfind"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        FAILED_BUILDS+=("jsfind")
        return
    fi

    cd "$JSFIND_PATH"

    log "npm run clean"
    npm run clean

    clean_json_resources "$JSFIND_PATH/data"

    cd -
}

clean_ktfind () {
    echo
    hdr "clean_ktfind"

    cd "$KTFIND_PATH"

    GRADLE=
    # check for gradle wrapper
    if [ -f "gradlew" ]
    then
        GRADLE="./gradlew"
    elif [ -n "$(which gradle)" ]
    then
        GRADLE="gradle"
    else
        log_error "You need to install gradle"
        FAILED_BUILDS+=("ktfind")
        cd -
        return
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    clean_json_resources "$KTFIND_PATH/src/main/resources"

    clean_test_resources "$KTFIND_PATH/src/test/resources"

    cd -
}

clean_objcfind () {
    echo
    hdr "clean_objcfind"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("objcfind")
        return
    fi

    cd "$OBJCFIND_PATH"

    log "swift package clean"
    swift package clean

    cd -
}

clean_mlfind () {
    echo
    hdr "clean_mlfind"

    # TODO: probably want to delete the _build directory
}

clean_plfind () {
    echo
    hdr "clean_plfind"

    clean_json_resources "$PLFIND_PATH/share"
}

clean_phpfind () {
    echo
    hdr "clean_phpfind"

    clean_json_resources "$PHPFIND_PATH/resources"
}

clean_ps1find () {
    echo
    hdr "clean_ps1find"
    log "Nothing to do for powershell"
}

clean_pyfind () {
    echo
    hdr "clean_pyfind"

    clean_json_resources "$PYFIND_PATH/pyfind/data"
}

clean_rbfind () {
    echo
    hdr "clean_rbfind"

    clean_json_resources "$RBFIND_PATH/data"

    clean_test_resources "$RBFIND_PATH/test/fixtures"
}

clean_rsfind () {
    echo
    hdr "clean_rsfind"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo"
        FAILED_BUILDS+=("rsfind")
        return
    fi

    cd "$RSFIND_PATH"

    echo "cargo clean"
    cargo clean

    cd -
}

clean_scalafind () {
    echo
    hdr "clean_scalafind"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        log_error "You need to install sbt"
        FAILED_BUILDS+=("scalafind")
        return
    fi

    # TODO: convert to sbt command

    cd "$SCALAFIND_PATH"

    log "sbt clean"
    sbt clean

    clean_json_resources "$SCALAFIND_PATH/src/main/resources"

    clean_test_resources "$SCALAFIND_PATH/src/test/resources"

    cd -
}

clean_swiftfind () {
    echo
    hdr "clean_swiftfind"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("swiftfind")
        return
    fi

    cd "$SWIFTFIND_PATH"

    log "swift package clean"
    swift package clean

    cd -
}

clean_tsfind () {
    echo
    hdr "clean_tsfind"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        FAILED_BUILDS+=("tsfind")
        return
    fi

    cd "$TSFIND_PATH"

    log "npm run clean"
    npm run clean

    clean_json_resources "$TSFIND_PATH/data"

    cd -
}

clean_linux () {
    hdr "clean_linux"

    clean_bashfind

    clean_cfind

    # clean_cljfind

    # clean_cppfind

    clean_csfind

    # clean_dartfind

    clean_fsfind

    clean_gofind

    clean_groovyfind

    # clean_hsfind

    clean_javafind

    clean_jsfind

    clean_ktfind

    # clean_objcfind

    # clean_mlfind

    clean_plfind

    clean_phpfind

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

HELP=
CLEAN_ALL=
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
            CLEAN_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "CLEAN_ALL: $CLEAN_ALL"
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
    print_failed_builds
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

print_failed_builds
