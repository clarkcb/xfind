#!/bin/bash
################################################################################
#
# unittest.sh
#
# Runs unit tests for specified language version of xfind, or all versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"


########################################
# Unit Test Functions
########################################

unittest_c () {
    echo
    hdr "unittest_c"

    log "Unit-testing cfind"
    cd "$CFIND_PATH"
    log "make run_tests"
    make run_tests
    cd -
}

unittest_clojure () {
    echo
    hdr "unittest_clojure"

    # Test with lein
    log "Unit-testing cljfind"
    cd "$CLJFIND_PATH"
    log "lein test"
    lein test
    cd -
}

unittest_cpp () {
    echo
    hdr "unittest_cpp"

    log "Unit-testing cppfind"
    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$CPPFIND_PATH/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]
        then
            CPPFIND_TEST_EXE=$CMAKE_BUILD_DIR/cppfind-tests
            log "$CPPFIND_TEST_EXE"
            $CPPFIND_TEST_EXE
        fi
    done
}

unittest_csharp () {
    echo
    hdr "unittest_csharp"

    # VERBOSITY=quiet
    # VERBOSITY=minimal
    VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing csfind"
    log "dotnet test $CSFIND_PATH/CsFind.sln --verbosity $VERBOSITY"
    dotnet test $CSFIND_PATH/CsFind.sln --verbosity $VERBOSITY
}

unittest_dart () {
    echo
    hdr "unittest_dart"

    cd "$DARTFIND_PATH"
    log "Unit-testing dartfind"
    log "pub run test"
    pub run test
    cd -
}

unittest_fsharp () {
    echo
    hdr "unittest_fsharp"

    # VERBOSITY=quiet
    # VERBOSITY=minimal
    VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing fsfind"
    log "dotnet test $FSFIND_PATH/FsFind.sln --verbosity $VERBOSITY"
    dotnet test $FSFIND_PATH/FsFind.sln --verbosity $VERBOSITY
}

unittest_go () {
    echo
    hdr "unittest_go"

    # Run the tests using go test
    log "Unit-testing gofind"
    cd "$GOFIND_PATH"
    log "go test --cover ./..."
    # cd "$GOSRC_PATH"; go test; cd -
    go test --cover ./...
    cd -
}

unittest_haskell () {
    echo
    hdr "unittest_haskell"

    # test with stack
    log "Unit-testing hsfind"
    log "stack test"
    cd "$HSFIND_PATH"; stack test; cd -
}

unittest_java () {
    echo
    hdr "unittest_java"

    # run tests via maven
    log "Unit-testing javafind"
    log "mvn -f $JAVAFIND_PATH/pom.xml test"
    mvn -f "$JAVAFIND_PATH/pom.xml" test
}

unittest_javascript () {
    echo
    hdr "unittest_javascript"

    # run tests
    log "Unit-testing jsfind"
    cd "$JSFIND_PATH"
    log "npm test"
    npm test
    cd -
}

unittest_kotlin () {
    echo
    hdr "unittest_kotlin"

    # run tests via gradle
    log "Unit-testing ktfind"
    log "gradle -b $KTFIND_PATH/build.gradle test"
    gradle -b "$KTFIND_PATH/build.gradle" test
}

unittest_objc () {
    echo
    hdr "unittest_objc"

    cd "$OBJCFIND_PATH"
    log "Unit-testing objcfind"
    log "xcodebuild test -project objcfind.xcodeproj -scheme objcfind"
    xcodebuild test -project objcfind.xcodeproj -scheme objcfind
    cd -
}

unittest_ocaml () {
    echo
    hdr "unittest_ocaml"

    cd "$MLFIND_PATH"
    log "Unit-testing mlfind"
    ./unittest.sh
    cd -
}

unittest_perl () {
    echo
    hdr "unittest_perl"

    TESTS_PATH=$PLFIND_PATH/t

    # run tests using Test::Simple
    log "Unit-testing plfind"
    FILES=$(find "$TESTS_PATH" -name "*_test.pl")
    for f in ${FILES[*]}
    do
        log "perl $f"
        perl "$f"
    done
}

unittest_php () {
    echo
    hdr "unittest_php"

    TESTS_PATH="$PHPFIND_PATH/tests"
    PHPUNIT="$PHPFIND_PATH/vendor/bin/phpunit"

    if [ ! -f "$PHPUNIT" ]
    then
        echo "You need to install phpunit"
        return
    fi

    # run tests with phpunit
    log "Unit-testing phpfind"
    log "$PHPUNIT $TESTS_PATH"
    "$PHPUNIT" "$TESTS_PATH"
}

unittest_python () {
    echo
    hdr "unittest_python"

    TESTS_PATH="$PYFIND_PATH/tests"
    VENV_PATH="$PYFIND_PATH/venv"
    PYTHON="$VENV_PATH/bin/python"
    export PYTHONPATH="$PYTHON_PATH"

    if [ ! -d "$VENV_PATH" ]
    then
        log "venv path not found, you probably need to run the python build (./build.sh python)"
        return
    fi

    cd "$PYFIND_PATH"

    # activate the virtualenv
    log "source ./venv/bin/activate"
    source ./venv/bin/activate

    # Run the individual tests
    log "Unit-testing pyfind"
    log "nosetests"
    nosetests

    # deactivate the virtualenv
    log "deactivate"
    deactivate

    cd -
}

unittest_ruby () {
    echo
    hdr "unittest_ruby"

    log "Unit-testing rbfind"

    # Run all tests via rake
    cd "$RBFIND_PATH"
    log "rake test"
    rake test
    cd -
}

unittest_rust () {
    echo
    hdr "unittest_rust"

    # Run cargo test
    log "Unit-testing rsfind"
    cd "$RSFIND_PATH"
    log "cargo test"
    cargo test
    cd -
}

unittest_scala () {
    echo
    hdr "unittest_scala"

    # run tests via sbt
    log "Unit-testing scalafind"
    cd "$SCALAFIND_PATH"
    log "sbt test"
    sbt test
    cd -
}

unittest_swift () {
    echo
    hdr "unittest_swift"

    log "Unit-testing swiftfind"
    cd "$SWIFTFIND_PATH"
    log "swift test"
    swift test
    cd -
}

unittest_typescript () {
    echo
    hdr "unittest_typescript"

    # run tests
    log "Unit-testing tsfind"
    cd "$TSFIND_PATH"
    log "npm test"
    npm test
    cd -
}

unittest_all () {
    hdr "unittest_all"

    unittest_c

    unittest_clojure

    unittest_cpp

    unittest_csharp

    unittest_dart

    unittest_fsharp

    unittest_go

    unittest_haskell

    unittest_java

    unittest_javascript

    unittest_kotlin

    unittest_objc

    unittest_ocaml

    unittest_perl

    unittest_php

    unittest_python

    unittest_ruby

    unittest_rust

    unittest_scala

    unittest_swift

    unittest_typescript
}


########################################
# Unit-testing main
########################################

if [ $# == 0 ]
then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]
then
    unittest_all
elif [ "$ARG" == "c" ]
then
    unittest_c
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    unittest_clojure
elif [ "$ARG" == "cpp" ]
then
    unittest_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    unittest_csharp
elif [ "$ARG" == "dart" ]
then
    unittest_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    unittest_fsharp
elif [ "$ARG" == "go" ]
then
    unittest_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    unittest_haskell
elif [ "$ARG" == "java" ]
then
    unittest_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    unittest_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    unittest_kotlin
elif [ "$ARG" == "objc" ]
then
    unittest_objc
elif [ "$ARG" == "ocaml" ] || [ "$ARG" == "ml" ]
then
    unittest_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    unittest_perl
elif [ "$ARG" == "php" ]
then
    unittest_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    unittest_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    unittest_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    unittest_rust
elif [ "$ARG" == "scala" ]
then
    unittest_scala
elif [ "$ARG" == "swift" ]
then
    unittest_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    unittest_typescript
else
    echo "ERROR: unknown unittest argument: $ARG"
fi
