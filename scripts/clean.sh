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


########################################
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: clean.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Clean Functions
########################################

clean_c () {
    echo
    hdr "clean_c"

    # ensure make is installed
    if [ -z "$(which make)" ]
    then
        echo "You need to install make"
        return
    fi

    cd "$CFIND_PATH"
    log "make clean"
    make clean
    cd -
}

clean_clojure () {
    echo
    hdr "clean_clojure"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        echo "You need to install lein"
        return
    fi

    cd "$CLJFIND_PATH"
    log "lein clean"
    lein clean
    cd -
}

clean_cpp () {
    echo
    hdr "clean_cpp"

    cd "$CPPFIND_PATH"

    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        log "rm -rf $CPPFIND_PATH/cmake-build-$c"
        rm -rf "$CPPFIND_PATH/cmake-build-$c"
    done

    cd -
}

clean_csharp () {
    echo
    hdr "clean_csharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    cd "$CSFIND_PATH"
    log "dotnet clean"
    dotnet clean

    PROJECTS=(CsFind CsFindGen CsFindLib CsFindTests)
    for p in ${PROJECTS[*]}
    do
        if [ -d "$CSFIND_PATH/$p" ]
        then
            log "rm -rf $CSFIND_PATH/$p/bin"
            rm -rf "$CSFIND_PATH/$p/bin"

            log "rm -rf $CSFIND_PATH/$p/obj"
            rm -rf "$CSFIND_PATH/$p/obj"
        fi
    done

    cd -
}

clean_dart () {
    echo
    hdr "clean_dart"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        echo "You need to install dart"
        return
    fi

    # pub cache repair is apparently the closest thing to clean for dart
    cd "$DARTFIND_PATH"
    log "dart pub cache repair"
    dart pub cache repair
    cd -
}

clean_elixir () {
    echo
    hdr "clean_elixir"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        return
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        return
    fi

    cd "$EXFIND_PATH"

    log "mix clean"
    mix clean

    cd -
}

clean_fsharp () {
    echo
    hdr "clean_fsharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    cd "$FSFIND_PATH"
    log "dotnet clean"
    dotnet clean

    PROJECTS=(FsFind FsFindGen FsFindLib FsFindTests)
    for p in ${PROJECTS[*]}
    do
        if [ -d "$FSFIND_PATH/$p" ]
        then
            log "rm -rf $FSFIND_PATH/$p/bin"
            rm -rf "$FSFIND_PATH/$p/bin"

            log "rm -rf $FSFIND_PATH/$p/obj"
            rm -rf "$FSFIND_PATH/$p/obj"
        fi
    done

    cd -
}

clean_go () {
    echo
    hdr "clean_go"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        return
    fi

    cd "$GOFIND_PATH"
    log "go clean"
    go clean
    cd -
}

clean_groovy () {
    echo
    hdr "clean_groovy"

    # ensure gradle is installed
    if [ -z "$(which gradle)" ]
    then
        echo "You need to install gradle"
        return
    fi

    cd "$GROOVYFIND_PATH"
    log "gradle --warning-mode all clean"
    gradle --warning-mode all clean
    cd -
}

clean_haskell () {
    echo
    hdr "clean_haskell"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        echo "You need to install stack"
        return
    fi

    cd "$HSFIND_PATH"
    log "stack clean"
    stack clean
    cd -
}

clean_java () {
    echo
    hdr "clean_java"

    # ensure mvn is installed
    if [ -z "$(which mvn)" ]
    then
        echo "You need to install mvn"
        return
    fi

    log "mvn -f $JAVAFIND_PATH/pom.xml clean"
    mvn -f "$JAVAFIND_PATH/pom.xml" clean
}

clean_javascript () {
    echo
    hdr "clean_javascript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

    cd "$JSFIND_PATH"
    log "npm run clean"
    npm run clean
    cd -
}

clean_kotlin () {
    echo
    hdr "clean_kotlin"

    # ensure gradle is installed
    if [ -z "$(which gradle)" ]
    then
        echo "You need to install gradle"
        return
    fi

    cd "$KTFIND_PATH"
    log "gradle --warning-mode all clean"
    gradle --warning-mode all clean
    cd -
}

clean_objc () {
    echo
    hdr "clean_objc"

    cd "$OBJCFIND_PATH"
    log "swift package clean"
    swift package clean
    cd -
}

clean_ocaml () {
    echo
    hdr "clean_ocaml"

    # TODO: probably want to delete the _build directory
}

clean_perl () {
    echo
    hdr "clean_perl"
    log "Nothing to do for perl"
}

clean_php () {
    echo
    hdr "clean_php"
    log "Nothing to do for php"
}

clean_powershell () {
    echo
    hdr "clean_powershell"
    log "Nothing to do for powershell"
}

clean_python () {
    echo
    hdr "clean_python"
    log "Nothing to do for python"
}

clean_ruby () {
    echo
    hdr "clean_ruby"
    log "Nothing to do for ruby"
}

clean_rust () {
    echo
    hdr "clean_rust"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        echo "You need to install cargo"
        return
    fi

    cd "$RSFIND_PATH"
    echo "cargo clean"
    cargo clean
    cd -
}

clean_scala () {
    echo
    hdr "clean_scala"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        echo "You need to install sbt"
        return
    fi

    # TODO: convert to sbt command

    cd "$SCALAFIND_PATH"
    log "sbt clean"
    sbt clean
    cd -
}

clean_swift () {
    echo
    hdr "clean_swift"

    cd "$SWIFTFIND_PATH"
    log "swift package clean"
    swift package clean
    cd -
}

clean_typescript () {
    echo
    hdr "clean_typescript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

    cd "$TSFIND_PATH"
    log "npm run clean"
    npm run clean
    cd -
}

clean_linux () {
    hdr "clean_linux"

    clean_c

    # clean_clojure

    # clean_cpp

    clean_csharp

    # clean_dart

    clean_fsharp

    clean_go

    # clean_haskell

    clean_java

    clean_javascript

    clean_kotlin

    # clean_objc

    # clean_ocaml

    clean_perl

    clean_php

    clean_python

    clean_ruby

    clean_rust

    # clean_scala

    clean_swift

    clean_typescript
}

clean_all () {
    hdr "clean_all"

    clean_c

    clean_clojure

    clean_cpp

    clean_csharp

    clean_dart

    clean_fsharp

    clean_go

    clean_haskell

    clean_java

    clean_javascript

    clean_kotlin

    clean_objc

    clean_ocaml

    clean_perl

    clean_php

    clean_powershell

    clean_python

    clean_ruby

    clean_rust

    clean_scala

    clean_swift

    clean_typescript
}


########################################
# Clean Main
########################################
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
log "TARGET_LANGS: ${TARGET_LANGS[*]}"

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$CLEAN_ALL" ]
then
    clean_all
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
        c)
            clean_c
            ;;
        clj | clojure)
            clean_clojure
            ;;
        cpp)
            clean_cpp
            ;;
        cs | csharp)
            clean_csharp
            ;;
        dart)
            clean_dart
            ;;
        elixir | ex)
            clean_elixir
            ;;
        fs | fsharp)
            clean_fsharp
            ;;
        go)
            clean_go
            ;;
        groovy)
            clean_groovy
            ;;
        haskell | hs)
            clean_haskell
            ;;
        java)
            clean_java
            ;;
        javascript | js)
            clean_javascript
            ;;
        kotlin | kt)
            clean_kotlin
            ;;
        objc)
            clean_objc
            ;;
        # ocaml | ml)
        #     clean_ocaml
        #     ;;
        perl | pl)
            clean_perl
            ;;
        php)
            clean_php
            ;;
        ps1 | powershell)
            clean_powershell
            ;;
        py | python)
            clean_python
            ;;
        rb | ruby)
            clean_ruby
            ;;
        rs | rust)
            clean_rust
            ;;
        scala)
            clean_scala
            ;;
        swift)
            clean_swift
            ;;
        ts | typescript)
            clean_typescript
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done
