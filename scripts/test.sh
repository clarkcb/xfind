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

test_lang_version () {
    local lang_version="$1"
    log "$lang_version $FIND_PARAMS"
    time "$lang_version" $EXTS $DEBUG $PATH
}

test_clojure () {
    hdr "test_clojure"
    test_lang_version "cljfind"
}

test_cpp () {
    hdr "test_cpp"
    test_lang_version "cppfind"
}

test_csharp () {
    hdr "test_csharp"
    test_lang_version "csfind"
}

test_dart () {
    hdr "test_dart"
    test_lang_version "dartfind"
}

test_fsharp () {
    hdr "test_fsharp"
    test_lang_version "fsfind"
}

test_go () {
    hdr "test_go"
    test_lang_version "gofind"
}

test_haskell () {
    hdr "test_haskell"
    test_lang_version "hsfind"
}

test_java () {
    hdr "test_java"
    test_lang_version "javafind"
}

test_javascript () {
    hdr "test_javascript"
    test_lang_version "jsfind"
}

test_kotlin () {
    hdr "test_kotlin"
    test_lang_version "ktfind"
}

test_objc () {
    hdr "test_objc"
    test_lang_version "objcfind"
}

test_ocaml () {
    hdr "test_ocaml"
    test_lang_version "ocamlfind"
}

test_perl () {
    hdr "test_perl"
    test_lang_version "plfind"
}

test_php () {
    hdr "test_php"
    test_lang_version "phpfind"
}

test_python () {
    hdr "test_python"
    test_lang_version "pyfind"
}

test_ruby () {
    hdr "test_ruby"
    test_lang_version "rbfind"
}

test_rust () {
    hdr "test_rust"
    test_lang_version "rsfind"
}

test_scala () {
    hdr "test_scala"
    test_lang_version "scalafind"
}

test_swift () {
    hdr "test_swift"
    test_lang_version "swiftfind"
}

test_typescript () {
    hdr "test_typescript"
    test_lang_version "tsfind"
}

test_all () {
    hdr "test_all"

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
