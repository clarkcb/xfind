#!/bin/sh
################################################################################
#
# test.sh
#
# Runs and times a common find across the versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"

EXTS="-x py"
DEBUG=""
PATH=$XFIND_PATH/python

FIND_PARAMS="$EXTS $DEBUG $PATH"


########################################
# Test Functions
########################################

test_clojure () {
    hdr "test_clojure"
    log "cljfind $FIND_PARAMS"
    time cljfind $EXTS $DEBUG $PATH
}

test_cpp () {
    hdr "test_cpp"
    log "cppfind $FIND_PARAMS"
    time cppfind $EXTS $DEBUG $PATH
}

test_csharp () {
    hdr "test_csharp"
    log "csfind $FIND_PARAMS"
    time csfind $EXTS $DEBUG $PATH
}

test_dart () {
    hdr "test_dart"
    log "dartfind $FIND_PARAMS"
    time dartfind $EXTS $DEBUG $PATH
}

test_fsharp () {
    hdr "test_fsharp"
    log "fsfind $FIND_PARAMS"
    time fsfind $EXTS $DEBUG $PATH
}

test_go () {
    hdr "test_go"
    log "gofind $FIND_PARAMS"
    time gofind $EXTS $DEBUG $PATH
}

test_haskell () {
    hdr "test_haskell"
    log "hsfind $FIND_PARAMS"
    time hsfind $EXTS $DEBUG $PATH
}

test_java () {
    hdr "test_java"
    log "javafind $FIND_PARAMS"
    time javafind $EXTS $DEBUG $PATH
}

test_javascript () {
    hdr "test_javascript"
    log "jsfind $FIND_PARAMS"
    time jsfind $EXTS $DEBUG $PATH
}

test_kotlin () {
    hdr "test_kotlin"
    log "ktfind $FIND_PARAMS"
    time ktfind $EXTS $DEBUG $PATH
}

test_objc () {
    hdr "test_objc"
    log "objcfind $FIND_PARAMS"
    time objcfind $EXTS $DEBUG $PATH
}

test_ocaml () {
    hdr "test_ocaml"
    log "ocamlfind $FIND_PARAMS"
    time ocamlfind $EXTS $DEBUG $PATH
}

test_perl () {
    hdr "test_perl"
    log "plfind $FIND_PARAMS"
    time plfind $EXTS $DEBUG $PATH
}

test_php () {
    hdr "test_php"
    log "phpfind $FIND_PARAMS"
    time phpfind $EXTS $DEBUG $PATH
}

test_python () {
    hdr "test_python"
    log "pyfind.py $FIND_PARAMS"
    time pyfind.py $EXTS $DEBUG $PATH
}

test_ruby () {
    hdr "test_ruby"
    log "rbfind $FIND_PARAMS"
    time rbfind $EXTS $DEBUG $PATH
}

test_rust () {
    hdr "test_rust"
    log "rsfind $FIND_PARAMS"
    time rsfind $EXTS $DEBUG $PATH
}

test_scala () {
    hdr "test_scala"
    log "scalafind $FIND_PARAMS"
    time scalafind $EXTS $DEBUG $PATH
}

test_swift () {
    hdr "test_swift"
    log "swiftfind $FIND_PARAMS"
    time swiftfind $EXTS $DEBUG $PATH
}

test_typescript () {
    hdr "test_typescript"
    log "tsfind $FIND_PARAMS"
    time tsfind $EXTS $DEBUG $PATH
}

test_all () {
    log "test_all"

    test_clojure

    test_cpp

    test_csharp

    test_dart

    test_fsharp

    test_go

    test_haskell

    test_java

    test_javascript

    test_kotlin

    test_objc

    test_ocaml

    test_perl

    test_php

    test_python

    test_ruby

    test_rust

    test_scala

    test_swift

    test_typescript
}


########################################
# Test Steps
########################################

if [ $# == 0 ]
then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]
then
    test_all
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    test_clojure
elif [ "$ARG" == "cpp" ]
then
    test_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    test_csharp
elif [ "$ARG" == "dart" ]
then
    test_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    test_fsharp
elif [ "$ARG" == "go" ]
then
    test_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    test_haskell
elif [ "$ARG" == "java" ]
then
    test_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    test_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    test_kotlin
elif [ "$ARG" == "objc" ]
then
    test_objc
elif [ "$ARG" == "ocaml" ]
then
    test_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    test_perl
elif [ "$ARG" == "php" ]
then
    test_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    test_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    test_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    test_rust
elif [ "$ARG" == "scala" ]
then
    test_scala
elif [ "$ARG" == "swift" ]
then
    test_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    test_typescript
else
    echo "ERROR: unknown test argument: $ARG"
fi
