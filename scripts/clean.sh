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
# Clean Functions
########################################

clean_c () {
    echo
    hdr "clean_c"

    cd "$CFIND_PATH"
    log "make clean"
    make clean
    cd -
}

clean_clojure () {
    echo
    hdr "clean_clojure"

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

    PROJECTS=(CsFind CsFindGen CsFindLib CsFindTests)
    SUBDIRS=(bin obj)
    for p in ${PROJECTS[*]}
    do
        for d in ${SUBDIRS[*]}
        do
            log "rm -rf $CSFIND_PATH/$p/$d"
            rm -rf "$CSFIND_PATH/$p/$d"
        done
    done
}

clean_dart () {
    echo
    hdr "clean_dart"

    # pub cache repair is apparently the closest thing to clean for dart
    cd "$DARTFIND_PATH"
    log "dart pub cache repair"
    dart pub cache repair
    cd -
}

clean_fsharp () {
    echo
    hdr "clean_fsharp"

    PROJECTS=(FsFind FsFindLib FsFindTests)
    SUBDIRS=(bin obj)
    for p in ${PROJECTS[*]}
    do
        for d in ${SUBDIRS[*]}
        do
            log "rm -rf $FSFIND_PATH/$p/$d"
            rm -rf "$FSFIND_PATH/$p/$d"
        done
    done
}

clean_go () {
    echo
    hdr "clean_go"

    cd "$GOFIND_PATH"
    log "go clean"
    go clean
    cd -
}

clean_haskell () {
    echo
    hdr "clean_haskell"

    cd "$HSFIND_PATH"
    log "stack clean"
    stack clean
    cd -
}

clean_java () {
    echo
    hdr "clean_java"

    log "mvn -f $JAVAFIND_PATH/pom.xml clean"
    mvn -f "$JAVAFIND_PATH/pom.xml" clean
}

clean_javascript () {
    echo
    hdr "clean_javascript"

    cd "$JSFIND_PATH"
    log "npm run clean"
    npm run clean
    cd -
}

clean_kotlin () {
    echo
    hdr "clean_kotlin"

    cd "$KTFIND_PATH"
    log "gradle --warning-mode all clean"
    gradle --warning-mode all clean
    cd -
}

clean_objc () {
    echo
    hdr "clean_objc"

    # TODO: is there a clean command for xcodebuild?
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

    cd "$RSFIND_PATH"
    echo "cargo clean"
    cargo clean
    cd -
}

clean_scala () {
    echo
    hdr "clean_scala"

    # TODO: convert to sbt command

    cd "$SCALAFIND_PATH"
    log "sbt clean"
    sbt clean
    cd -
}

clean_swift () {
    echo
    hdr "clean_swift"

    # TODO: is there a clean command for swift?
}

clean_typescript () {
    echo
    hdr "clean_typescript"

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

    clean_dart

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
elif [ "$ARG" == "linux" ]
then
    clean_linux
elif [ "$ARG" == "c" ]
then
    clean_c
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
