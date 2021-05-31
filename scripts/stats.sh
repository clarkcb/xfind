#!/bin/sh
################################################################################
#
# stats.sh
#
# Get code file stats for the various languages
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

# word_counts
word_counts () {
    local files="$@" lines=0 words=0 chars=0
    for f in ${files[*]}; do
        # log "wc $f"
        WC=$(wc $f)
        # echo "$WC"
        ARR=($WC)
        lines=$(($lines + ${ARR[0]}))
        words=$(($words + ${ARR[1]}))
        chars=$(($chars + ${ARR[2]}))
    done
    log "lines: $lines"
    log "words: $words"
    log "chars: $chars"
}

########################################
# Build Functions
########################################

stats_c () {
    echo
    hdr "stats_c"
    # find ./ -type f \( -iname \*.jpg -o -iname \*.png \)
    CFIND_INCPATH=$CFIND_PATH/include
    CFILES=$(find $CFIND_INCPATH -type f -iname "*.h")
    log "Main include counts"
    word_counts $CFILES
    CFIND_SRCPATH=$CFIND_PATH/src
    CFILES=$(find $CFIND_SRCPATH -type f \( -iname \*.h -o -iname \*.c \))
    log "Main source counts"
    word_counts $CFILES
    CFIND_TESTPATH=$CFIND_PATH/tests
    CFILES=$(find $CFIND_TESTPATH -type f -iname "*.c")
    log "Test source counts"
    word_counts $CFILES
}

stats_clojure () {
    echo
    hdr "stats_clojure"
    CLJFIND_SRCPATH=$CLJFIND_PATH/src
    CLJFILES=$(find $CLJFIND_SRCPATH -type f -iname "*.clj")
    log "Main source counts"
    word_counts $CLJFILES
    CLJFIND_TESTPATH=$CLJFIND_PATH/test
    CLJFILES=$(find $CLJFIND_TESTPATH -type f -iname "*.clj")
    log "Test counts"
    word_counts $CLJFILES
}

stats_cpp () {
    echo
    hdr "stats_cpp"
    # find ./ -type f \( -iname \*.jpg -o -iname \*.png \)
    CPPFIND_INCPATH=$CPPFIND_PATH/include
    CPPFILES=$(find $CPPFIND_INCPATH -type f -iname "*.h")
    log "Main include counts"
    word_counts $CPPFILES
    CPPFIND_SRCPATH=$CPPFIND_PATH/src
    CPPFILES=$(find $CPPFIND_SRCPATH -type f \( -iname \*.h -o -iname \*.cpp \))
    log "Main source counts"
    word_counts $CPPFILES
    CPPFIND_TESTPATH=$CPPFIND_PATH/tests
    CPPFILES=$(find $CPPFIND_TESTPATH -type f -iname "*.cpp")
    log "Test source counts"
    word_counts $CPPFILES
}

stats_csharp () {
    echo
    hdr "stats_csharp"
    CSFIND_SRCPATH=$CSFIND_PATH/CsFind
    CSFILES=$(find $CSFIND_SRCPATH -type f -iname "*.cs" | grep -v /obj/)
    log "Main source counts"
    word_counts $CSFILES
    CSFIND_TESTPATH=$CSFIND_PATH/CsFindTests
    CSFILES=$(find $CSFIND_TESTPATH -type f -iname "*.cs" | grep -v /obj/)
    log "Test source counts"
    word_counts $CSFILES
}

stats_dart () {
    echo
    hdr "stats_dart"
    DARTFIND_SRCPATH=$DARTFIND_PATH/lib
    DARTFILES=$(find $DARTFIND_SRCPATH -type f -iname "*.dart")
    log "Main source counts"
    word_counts $DARTFILES
    DARTFIND_TESTPATH=$DARTFIND_PATH/test
    DARTFILES=$(find $DARTFIND_TESTPATH -type f -iname "*.dart")
    log "Test source counts"
    word_counts $DARTFILES
}

stats_fsharp () {
    echo
    hdr "stats_fsharp"
    FSFIND_SRCPATH=$FSFIND_PATH/FsFind
    FSFILES=$(find $FSFIND_SRCPATH -type f -iname "*.fs")
    log "Main source counts"
    word_counts $FSFILES
    FSFIND_TESTPATH=$FSFIND_PATH/FsFindTests
    FSFILES=$(find $FSFIND_TESTPATH -type f -iname "*.fs" | grep -v /obj/)
    log "Test source counts"
    word_counts $FSFILES
}

stats_go () {
    echo
    hdr "stats_go"
    GOFILES=$(find $GOFIND_PATH -type f -iname "*.go" | grep -v "_test")
    log "Main source counts"
    word_counts $GOFILES
    GOFILES=$(find $GOFIND_PATH -type f -iname "*_test.go")
    log "Test source counts"
    word_counts $GOFILES
}

stats_haskell () {
    echo
    hdr "stats_haskell"
    HSFIND_SRCPATH=$HSFIND_PATH/src
    HSSRCFILES=$(find $HSFIND_SRCPATH -type f -iname "*.hs")
    log "Main source counts"
    word_counts $HSSRCFILES
    HSFIND_TESTPATH=$HSFIND_PATH/test
    HSTESTFILES=$(find $HSFIND_TESTPATH -type f -iname "*.hs")
    log "Test source counts"
    word_counts $HSTESTFILES
}

stats_java () {
    echo
    hdr "stats_java"
    JAVAFIND_SRCPATH=$JAVAFIND_PATH/src/main
    JAVASRCFILES=$(find $JAVAFIND_SRCPATH -type f -iname "*.java")
    log "Main source counts"
    word_counts $JAVASRCFILES
    JAVAFIND_TESTPATH=$JAVAFIND_PATH/src/test
    JAVATESTFILES=$(find $JAVAFIND_TESTPATH -type f -iname "*.java")
    log "Test source counts"
    word_counts $JAVATESTFILES
}

stats_javascript () {
    echo
    hdr "stats_javascript"
    JSFIND_SRCPATH=$JSFIND_PATH/src
    JSFILES=$(find $JSFIND_SRCPATH -type f -iname "*.js")
    log "Main source counts"
    word_counts $JSFILES
    JSFIND_TESTPATH=$JSFIND_PATH/tests
    JSTESTFILES=$(find $JSFIND_TESTPATH -type f -iname "*.js")
    log "Test source counts"
    word_counts $JSTESTFILES
}

stats_kotlin () {
    echo
    hdr "stats_kotlin"
    KTFIND_SRCPATH=$KTFIND_PATH/src/main
    KTFILES=$(find $KTFIND_SRCPATH -type f -iname "*.kt")
    log "Main source counts"
    word_counts $KTFILES
    KTFIND_TESTPATH=$KTFIND_PATH/src/test
    KTTESTFILES=$(find $KTFIND_TESTPATH -type f -iname "*.kt")
    log "Test source counts"
    word_counts $KTTESTFILES
}

stats_objc () {
    echo
    hdr "stats_objc"
    OBJCFIND_SRCPATH=$OBJCFIND_PATH/objcfind
    OBJCFILES=$(find $OBJCFIND_SRCPATH -type f \( -iname \*.h -o -iname \*.m \))
    log "Main source counts"
    word_counts $OBJCFILES
    OBJCFIND_TESTPATH=$OBJCFIND_PATH/objcfind_tests
    OBJCTESTFILES=$(find $OBJCFIND_TESTPATH -type f -iname "*.m")
    log "Test source counts"
    word_counts $OBJCTESTFILES
}

stats_perl () {
    echo
    hdr "stats_perl"
    PLFIND_SRCPATH=$PLFIND_PATH/lib
    PLSRCFILES=$(find $PLFIND_SRCPATH -type f -iname "*.p[lm]")
    log "Main source counts"
    word_counts $PLSRCFILES
    PLFIND_TESTPATH=$PLFIND_PATH/t
    PLTESTFILES=$(find $PLFIND_TESTPATH -type f -iname "*.p[lm]")
    log "Test source counts"
    word_counts $PLTESTFILES
}

stats_php () {
    echo
    hdr "stats_php"
    PHPFIND_SRCPATH=$PHPFIND_PATH/src
    PHPSRCFILES=$(find $PHPFIND_SRCPATH -type f -iname "*.php")
    log "Main source counts"
    word_counts $PHPSRCFILES
    PHPFIND_TESTPATH=$PHPFIND_PATH/tests
    PHPTESTFILES=$(find $PHPFIND_TESTPATH -type f -iname "*.php")
    log "Test source counts"
    word_counts $PHPTESTFILES
}

stats_python () {
    echo
    hdr "stats_python"
    PYFIND_SRCPATH=$PYFIND_PATH/pyfind
    PYSRCFILES=$(find $PYFIND_SRCPATH -type f -iname "*.py")
    log "Main source counts"
    word_counts $PYSRCFILES
    PYFIND_TESTPATH=$PYFIND_PATH/tests
    PYTESTFILES=$(find $PYFIND_TESTPATH -type f -iname "*.py")
    log "Test source counts"
    word_counts $PYTESTFILES
}

stats_ruby () {
    echo
    hdr "stats_ruby"
    RBFIND_SRCPATH=$RBFIND_PATH/lib
    RBSRCFILES=$(find $RBFIND_SRCPATH -type f -iname "*.rb")
    log "Main source counts"
    word_counts $RBSRCFILES
    RBFIND_TESTPATH=$RBFIND_PATH/test
    RBTESTFILES=$(find $RBFIND_TESTPATH -type f -iname "*.rb")
    log "Test source counts"
    word_counts $RBTESTFILES
}

stats_rust () {
    echo
    hdr "stats_rust"
    RSFIND_SRCPATH=$RSFIND_PATH/src
    RSSRCFILES=$(find $RSFIND_SRCPATH -type f -iname "*.rs")
    log "Source counts (tests are embedded)"
    word_counts $RSSRCFILES
}

stats_scala () {
    echo
    hdr "stats_scala"
    SCALAFIND_SRCPATH=$SCALAFIND_PATH/src/main
    SCALASRCFILES=$(find $SCALAFIND_SRCPATH -type f -iname "*.scala")
    log "Main source counts"
    word_counts $SCALASRCFILES
    SCALAFIND_TESTPATH=$SCALAFIND_PATH/src/test
    SCALATESTFILES=$(find $SCALAFIND_TESTPATH -type f -iname "*.scala")
    log "Test source counts"
    word_counts $SCALATESTFILES
}

stats_swift () {
    echo
    hdr "stats_swift"
    SWIFTFIND_SRCPATH=$SWIFTFIND_PATH/Sources
    SWIFTSRCFILES=$(find $SWIFTFIND_SRCPATH -type f -iname "*.swift")
    log "Main source counts"
    word_counts $SWIFTSRCFILES
    SWIFTFIND_TESTPATH=$SWIFTFIND_PATH/Tests
    SWIFTTESTFILES=$(find $SWIFTFIND_TESTPATH -type f -iname "*.swift")
    log "Test source counts"
    word_counts $SWIFTTESTFILES
}

stats_typescript () {
    echo
    hdr "stats_typescript"
    TSFIND_SRCPATH=$TSFIND_PATH/src
    TSFILES=$(find $TSFIND_SRCPATH -type f -iname "*.ts")
    log "Main source counts"
    word_counts $TSFILES
    TSFIND_TESTPATH=$TSFIND_PATH/tests
    TSTESTFILES=$(find $TSFIND_TESTPATH -type f -iname "*.ts")
    log "Test source counts"
    word_counts $TSTESTFILES
}

stats_all () {
    hdr "stats_all"

    stats_c

    stats_clojure

    stats_cpp

    stats_csharp

    stats_dart

    stats_fsharp

    stats_go

    stats_haskell

    stats_java

    stats_javascript

    stats_kotlin

    stats_objc

    stats_perl

    stats_php

    stats_python

    stats_ruby

    stats_rust

    stats_scala

    stats_swift

    stats_typescript
}


########################################
# Build Steps
########################################

if [ $# == 0 ]
then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]
then
    stats_all
elif [ "$ARG" == "c" ]
then
    stats_c
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    stats_clojure
elif [ "$ARG" == "cpp" ]
then
    stats_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    stats_csharp
elif [ "$ARG" == "dart" ]
then
    stats_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    stats_fsharp
elif [ "$ARG" == "go" ] || [ "$ARG" == "go" ]
then
    stats_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    stats_haskell
elif [ "$ARG" == "java" ]
then
    stats_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    stats_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    stats_kotlin
elif [ "$ARG" == "objc" ]
then
    stats_objc
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    stats_perl
elif [ "$ARG" == "php" ] || [ "$ARG" == "php" ]
then
    stats_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    stats_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    stats_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    stats_rust
elif [ "$ARG" == "scala" ]
then
    stats_scala
elif [ "$ARG" == "swift" ]
then
    stats_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    stats_typescript
else
    echo "ERROR: unknown stats argument: $ARG"
fi
