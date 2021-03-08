#!/bin/sh
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
# Clean Functions
########################################

clean_clojure () {
    echo
    log "clean_clojure"
    CLJFIND_PATH=$CLOJURE_PATH/cljfind

    cd $CLJFIND_PATH
    log "lein clean"
    lein clean
    cd -
}

clean_cpp () {
    echo
    log "clean_cpp"
    CPPFIND_PATH=$CPP_PATH/cppfind

    cd $CPPFIND_PATH

    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR="cmake-build-$c"

        log "cmake --build $CMAKE_BUILD_DIR --target clean -- -W -Wall -Werror"
        cmake --build $CMAKE_BUILD_DIR --target clean -- -W -Wall -Werror
    done

    cd -
}

clean_csharp () {
    echo
    log "clean_csharp"
    CSFIND_PATH=$CSHARP_PATH/CsFind

    log "rm -rf $CSFIND_PATH/CsFind/bin"
    rm -rf "$CSFIND_PATH/CsFind/bin"

    log "rm -rf $CSFIND_PATH/CsFind/obj"
    rm -rf "$CSFIND_PATH/CsFind/obj"

    log "rm -rf $CSFIND_PATH/CsFindTests/bin"
    rm -rf "$CSFIND_PATH/CsFindTests/bin"

    log "rm -rf $CSFIND_PATH/CsFindTests/obj"
    rm -rf "$CSFIND_PATH/CsFindTests/obj"
}

clean_dart () {
    echo
    log "clean_dart"
    DARTFIND_PATH=$DART_PATH/dartfind

    # pub cache repair is apparently the closest thing to clean for dart
    cd $DARTFIND_PATH
    log "dart pub cache repair"
    dart pub cache repair
    cd -
}

clean_fsharp () {
    echo
    log "clean_fsharp"
    FSFIND_PATH=$FSHARP_PATH/FsFind

    log "rm -rf $FSFIND_PATH/FsFind/bin"
    rm -rf "$FSFIND_PATH/FsFind/bin"

    log "rm -rf $FSFIND_PATH/FsFind/obj"
    rm -rf "$FSFIND_PATH/FsFind/obj"

    log "rm -rf $FSFIND_PATH/FsFindTests/bin"
    rm -rf "$FSFIND_PATH/FsFindTests/bin"

    log "rm -rf $FSFIND_PATH/FsFindTests/obj"
    rm -rf "$FSFIND_PATH/FsFindTests/obj"
}

clean_go () {
    echo
    log "clean_go"

    cd $GOFIND_PATH
    log "go clean"
    go clean
}

clean_haskell () {
    echo
    log "clean_haskell"

    cd $HSFIND_PATH
    log "stack clean"
    stack clean
}

clean_java () {
    echo
    log "clean_java"

    log "mvn -f $JAVAFIND_PATH/pom.xml clean"
    mvn -f $JAVAFIND_PATH/pom.xml clean
}

clean_javascript () {
    echo
    log "clean_javascript"

    cd $JSFIND_PATH
    log "npm run clean"
    npm run clean
    cd -
}

clean_kotlin () {
    echo
    log "clean_kotlin"

    cd $KTFIND_PATH/
    log "gradle -b build.gradle clean"
    gradle -b build.gradle clean
    cd -
}

clean_objc () {
    echo
    log "clean_objc"

    # TODO: is there a clean command for xcodebuild?
}

clean_ocaml () {
    echo
    log "clean_ocaml"

    # TODO: probably want to delete the _build directory
}

clean_perl () {
    echo
    log "clean_perl"
    log "Nothing to do for perl"
}

clean_php () {
    echo
    log "clean_php"
    log "Nothing to do for php"
}

clean_python () {
    echo
    log "clean_python"
    log "Nothing to do for python"
}

clean_ruby () {
    echo
    log "clean_ruby"
    log "Nothing to do for ruby"
}

clean_rust () {
    echo
    log "clean_rust"

    cd $RSFIND_PATH
    echo "cargo clean"
    cargo clean
    cd -
}

clean_scala () {
    echo
    log "clean_scala"

    # TODO: convert to sbt command

    cd $SCALAFIND_PATH
    log "sbt clean"
    sbt clean
    cd -
}

clean_swift () {
    echo
    log "clean_swift"

    # TODO: is there a clean command for swift?
}

clean_typescript () {
    echo
    log "clean_typescript"

    cd $TSFIND_PATH
    log "npm run clean"
    npm run clean
    cd -
}

clean_all () {
    log "clean_all"

    clean_cpp

    clean_clojure

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

    clean_python

    clean_ruby

    clean_rust

    clean_scala

    clean_swift

    clean_typescript
}


########################################
# Clean Steps
########################################

if [ $# == 0 ]
then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]
then
    clean_all
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    clean_clojure
elif [ "$ARG" == "cpp" ]
then
    clean_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    clean_csharp
elif [ "$ARG" == "dart" ]
then
    clean_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    clean_fsharp
elif [ "$ARG" == "go" ]
then
    clean_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    clean_haskell
elif [ "$ARG" == "java" ]
then
    clean_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    clean_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    clean_kotlin
elif [ "$ARG" == "objc" ]
then
    clean_objc
elif [ "$ARG" == "ocaml" ] || [ "$ARG" == "ml" ]
then
    clean_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    clean_perl
elif [ "$ARG" == "php" ]
then
    clean_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    clean_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    clean_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    clean_rust
elif [ "$ARG" == "scala" ]
then
    clean_scala
elif [ "$ARG" == "swift" ]
then
    clean_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    clean_typescript
else
    echo "ERROR: unknown clean argument: $ARG"
fi
