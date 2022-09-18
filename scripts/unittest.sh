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
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: unittest.sh [-h|--help] {\"all\" | langcode}\n"
    exit
}


########################################
# Unit Test Functions
########################################

unittest_c () {
    echo
    hdr "unittest_c"

    # ensure make is installed
    if [ -z "$(which make)" ]
    then
        echo "You need to install make"
        return
    fi

    log "Unit-testing cfind"
    cd "$CFIND_PATH"
    log "make run_tests"
    make run_tests
    cd -
}

unittest_clojure () {
    echo
    hdr "unittest_clojure"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        echo "You need to install lein"
        return
    fi

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

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

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

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

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

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        return
    fi

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

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        echo "You need to install stack"
        return
    fi

    # test with stack
    log "Unit-testing hsfind"
    log "stack test"
    cd "$HSFIND_PATH"; stack test; cd -
}

unittest_java () {
    echo
    hdr "unittest_java"

    # ensure mvn is installed
    if [ -z "$(which mvn)" ]
    then
        echo "You need to install mvn"
        return
    fi

    # run tests via maven
    log "Unit-testing javafind"
    log "mvn -f $JAVAFIND_PATH/pom.xml test"
    mvn -f "$JAVAFIND_PATH/pom.xml" test
}

unittest_javascript () {
    echo
    hdr "unittest_javascript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

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

    # ensure gradle is installed
    if [ -z "$(which gradle)" ]
    then
        echo "You need to install gradle"
        return
    fi

    cd $KTFIND_PATH
    # run tests via gradle
    log "Unit-testing ktfind"
    log "gradle --warning-mode all test"
    gradle --warning-mode all test
    cd -
}

unittest_objc () {
    echo
    hdr "unittest_objc"

    # ensure xcode is installed
    if [ -z "$(which xcodebuild)" ]
    then
        echo "You need to install xcode"
        return
    fi

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

    # ensure rake is installed
    if [ -z "$(which rake)" ]
    then
        echo "You need to install rake"
        return
    fi

    # Run all tests via rake
    cd "$RBFIND_PATH"
    log "rake test"
    rake test
    cd -
}

unittest_rust () {
    echo
    hdr "unittest_rust"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        echo "You need to install cargo"
        return
    fi

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

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        echo "You need to install sbt"
        return
    fi

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

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        echo "You need to install swift"
        return
    fi

    log "Unit-testing swiftfind"
    cd "$SWIFTFIND_PATH"
    log "swift test"
    swift test
    cd -
}

unittest_typescript () {
    echo
    hdr "unittest_typescript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        echo "You need to install npm"
        return
    fi

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
HELP=
ARG=all

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
        *)
            ARG=$1
            ;;
    esac
    shift || true
done

if [ -n "$HELP" ]
then
    usage
fi

case $ARG in
    all)
        unittest_all
        ;;
    linux)
        unittest_linux
        ;;
    c)
        unittest_c
        ;;
    clj | clojure)
        unittest_clojure
        ;;
    cpp)
        unittest_cpp
        ;;
    cs | csharp)
        unittest_csharp
        ;;
    dart)
        unittest_dart
        ;;
    fs | fsharp)
        unittest_fsharp
        ;;
    go)
        unittest_go
        ;;
    haskell | hs)
        unittest_haskell
        ;;
    java)
        unittest_java
        ;;
    javascript | js)
        unittest_javascript
        ;;
    kotlin | kt)
        unittest_kotlin
        ;;
    objc)
        unittest_objc
        ;;
    ocaml | ml)
        unittest_ocaml
        ;;
    perl | pl)
        unittest_perl
        ;;
    php)
        unittest_php
        ;;
    py | python)
        unittest_python
        ;;
    rb | ruby)
        unittest_ruby
        ;;
    rs | rust)
        unittest_rust
        ;;
    scala)
        unittest_scala
        ;;
    swift)
        unittest_swift
        ;;
    ts | typescript)
        unittest_typescript
        ;;
    *)
        log_error "ERROR: unknown unittest argument: $ARG"
        ;;
esac
