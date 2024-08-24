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
    echo -e "\nUsage: unittest.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Unit Test Functions
########################################

unittest_c () {
    echo
    hdr "unittest_c"

    # if cmake is installed, display version
    if [ -n "$(which cmake)" ]
    then
        # cmake --version output looks like this: cmake version 3.30.2
        CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
        log "cmake version: $CMAKE_VERSION"
    fi

    log "Unit-testing cfind"
    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$CFIND_PATH/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]
        then
            CFIND_TEST_EXE=$CMAKE_BUILD_DIR/cfind-tests
            if [ -e "$CFIND_TEST_EXE" ]
            then
                log "$CFIND_TEST_EXE"
                $CFIND_TEST_EXE
                # output=$(script -q tmpout $CFIND_TEST_EXE | tee /dev/tty)
                # lastline=$(echo "$output" | tail -n 2)
                # if [[ "$lastline" =~ All[[:space:]]tests[[:space:]]passed ]]
                # then
                #     log "All C unit tests passed"
                # else
                #     log_error "ERROR: C unit tests failed"
                # fi
            else
                log_error "cfind-tests not found: $CFIND_TEST_EXE"
            fi
        else
            log_error "cmake build directory not found: $CMAKE_BUILD_DIR"
        fi
    done
}

unittest_clojure () {
    echo
    hdr "unittest_clojure"

    # if clojure is installed, display version
    if [ -n "$(which clj)" ]
    then
        # clj -version output looks like this: Clojure CLI version 1.11.4.1474
        # CLOJURE_VERSION=$(clj -version | head -n 1 | cut -d ' ' -f 3)
        CLOJURE_VERSION=$(clj -version 2>&1)
        log "clojure version: $CLOJURE_VERSION"
    fi

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        echo "You need to install lein"
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION=$(lein version)
    log "lein version: $LEIN_VERSION"

    cd "$CLJFIND_PATH"

    # Test with lein
    log "Unit-testing cljfind"
    log "lein test"
    output=$(lein test | tee /dev/tty)
    lastline=$(echo "$output" | tail -n 1)
    if [[ "$lastline" == "0 failures, 0 errors." ]]
    then
        log "All clojure unit tests passed"
    else
        log_error "ERROR: clojure unit tests failed"
    fi

    cd -
}

unittest_cpp () {
    echo
    hdr "unittest_cpp"

    # if cmake is installed, display version
    if [ -n "$(which cmake)" ]
    then
        # cmake --version output looks like this: cmake version 3.30.2
        CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
        log "cmake version: $CMAKE_VERSION"
    fi

    log "Unit-testing cppfind"
    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$CPPFIND_PATH/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]
        then
            CPPFIND_TEST_EXE=$CMAKE_BUILD_DIR/cppfind-tests
            if [ -e "$CPPFIND_TEST_EXE" ]
            then
                log "$CPPFIND_TEST_EXE"
                output=$(script -q tmpout $CPPFIND_TEST_EXE | tee /dev/tty)
                lastline=$(echo "$output" | tail -n 2)
                if [[ "$lastline" =~ All[[:space:]]tests[[:space:]]passed ]]
                then
                    log "All C++ unit tests passed"
                else
                    log_error "ERROR: C++ unit tests failed"
                fi
            else
                log_error "cppfind-tests not found: $CPPFIND_TEST_EXE"
            fi
        else
            log_error "cmake build directory not found: $CMAKE_BUILD_DIR"
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

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # VERBOSITY=quiet
    VERBOSITY=minimal
    # VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing csfind"
    log "dotnet test $CSFIND_PATH/CsFind.sln --verbosity $VERBOSITY"
    dotnet test "$CSFIND_PATH/CsFind.sln" --verbosity $VERBOSITY
}

unittest_dart () {
    echo
    hdr "unittest_dart"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        return
    fi

    DART_VERSION=$(dart --version)
    log "dart version: $DART_VERSION"

    cd "$DARTFIND_PATH"

    log "Unit-testing dartfind"
    log "dart run test"
    dart run test

    cd -
}

unittest_elixir () {
    echo
    hdr "unittest_elixir"

    # if elixir is installed, display version
    if [ -n "$(which elixir)" ]
    then
        ELIXIR_VERSION=$(elixir --version | grep Elixir)
        log "elixir version: $ELIXIR_VERSION"
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        return
    fi

    MIX_VERSION=$(mix --version | grep Mix)
    log "mix version: $MIX_VERSION"

    cd "$EXFIND_PATH"

    # run tests
    log "Unit-testing exfind"
    log "mix test"
    mix test

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

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # VERBOSITY=quiet
    VERBOSITY=minimal
    # VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "Unit-testing fsfind"
    log "dotnet test $FSFIND_PATH/FsFind.sln --verbosity $VERBOSITY"
    dotnet test "$FSFIND_PATH/FsFind.sln" --verbosity $VERBOSITY
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

    GO_VERSION=$(go version | sed 's/go version //')
    # GO_VERSION=$(go version | head -n 1 | cut -d ' ' -f 3)
    log "go version: $GO_VERSION"

    # Run the tests using go test
    log "Unit-testing gofind"
    cd "$GOFIND_PATH"

    log "go test --cover ./..."
    # cd "$GOSRC_PATH"; go test; cd -
    output=$(go test --cover ./... | tee /dev/tty)
    lastline=$(echo "$output" | tail -n 1)
    # echo "lastline: \"$lastline\""
    if [[ "$lastline" =~ ^ok[[:space:]] ]]
    then
        log "All go unit tests passed"
    else
        log_error "ERROR: go unit tests failed"
    fi

    cd -
}

unittest_groovy () {
    echo
    hdr "unittest_groovy"

    # if groovy is installed, display version
    if [ -n "$(which groovy)" ]
    then
        GROOVY_VERSION=$(groovy --version)
        log "groovy version: $GROOVY_VERSION"
    fi

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
        return
    fi

    GRADLE_VERSION=$($GRADLE --version | grep Gradle)
    log "$GRADLE version: $GRADLE_VERSION"

    cd "$GROOVYFIND_PATH"

    # run tests via gradle
    log "Unit-testing groovyfind"
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    cd -
}

unittest_haskell () {
    echo
    hdr "unittest_haskell"

    # if ghc is installed, display version
    if [ -n "$(which ghc)" ]
    then
        GHC_VERSION=$(ghc --version)
        log "ghc version: $GHC_VERSION"
    fi

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        echo "You need to install stack"
        return
    fi

    STACK_VERSION=$(stack --version)
    log "stack version: $STACK_VERSION"

    cd "$HSFIND_PATH"
    
    # test with stack
    log "Unit-testing hsfind"
    log "stack test"
    stack test
    
    cd -
}

unittest_java () {
    echo
    hdr "unittest_java"

    # if java is installed, display version
    if [ -n "$(which java)" ]
    then
        JAVA_VERSION=$(java -version 2>&1 | head -n 1)
        log "java version: $JAVA_VERSION"
    fi

    # ensure mvn is installed
    if [ -z "$(which mvn)" ]
    then
        echo "You need to install mvn"
        return
    fi

    MVN_VERSION=$(mvn --version | head -n 1)
    log "mvn version: $MVN_VERSION"

    # run tests via maven
    log "Unit-testing javafind"
    log "mvn -f $JAVAFIND_PATH/pom.xml test"
    output=$(script -q tmpout mvn -f "$JAVAFIND_PATH/pom.xml" test | tee /dev/tty)
    lastlines=$(echo "$output" | tail -n 9)
    # echo "lastlines: \"$lastlines\""
    if [[ "$lastlines" =~ Tests[[:space:]]run:[[:space:]]+[0-9]+,[[:space:]]+Failures:[[:space:]]+0,[[:space:]]+Errors:[[:space:]]+0 ]]
    then
        log "All java unit tests passed"
    else
        log_error "ERROR: java unit tests failed"
    fi
}

unittest_javascript () {
    echo
    hdr "unittest_javascript"

    # if node is installed, display version
    if [ -n "$(which node)" ]
    then
        NODE_VERSION=$(node --version)
        log "node version: $NODE_VERSION"
    fi

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    cd "$JSFIND_PATH"

    # run tests via npm
    log "Unit-testing jsfind"
    log "npm test"
    npm test

    cd -
}

unittest_kotlin () {
    echo
    hdr "unittest_kotlin"

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
        return
    fi

    GRADLE_VERSION=$($GRADLE --version | grep '^Gradle')
    log "$GRADLE version: $GRADLE_VERSION"

    cd "$KTFIND_PATH"

    # run tests via gradle
    log "Unit-testing ktfind"
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    cd -
}

unittest_objc () {
    echo
    hdr "unittest_objc"

    # TODO: copy resource files locally?
    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    cd "$OBJCFIND_PATH"

    log "Unit-testing objcfind"
    log "swift test"
    swift test

    cd -
}

# unittest_ocaml () {
#     echo
#     hdr "unittest_ocaml"

#     cd "$MLFIND_PATH"
#     log "Unit-testing mlfind"
#     ./unittest.sh
#     cd -
# }

unittest_perl () {
    echo
    hdr "unittest_perl"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        log_error "You need to install perl"
        return
    fi

    PERL_VERSION="$(perl -e 'print $^V' | grep '^v5')"
    if [ -z $PERL_VERSION ]
    then
        log_error "A 5.x version of perl is required"
        return
    fi

    log "perl version: $PERL_VERSION"

    TESTS_PATH="$PLFIND_PATH/t"

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

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        return
    fi

    # PHP_VERSION=$(php -r "echo phpversion();")
    PHP_VERSION=$(php -v | grep '^PHP [78]')
    if [ -z "$PHP_VERSION" ]
    then
        log_error "A version of PHP >= 7.x is required"
        return
    fi
    log "php version: $PHP_VERSION"

    # if composer is installed, display version
    if [ -n "$(which composer)" ]
    then
        COMPOSER_VERSION=$(composer --version 2>&1 | grep '^Composer')
        log "composer version: $COMPOSER_VERSION"
    fi

    TESTS_PATH="$PHPFIND_PATH/tests"
    PHPUNIT="$PHPFIND_PATH/vendor/bin/phpunit"

    if [ ! -f "$PHPUNIT" ]
    then
        echo "You need to install phpunit (build.sh php)"
        return
    fi

    # run tests with phpunit
    log "Unit-testing phpfind"
    log "$PHPUNIT $TESTS_PATH"
    "$PHPUNIT" "$TESTS_PATH"
}

unittest_powershell () {
    echo
    hdr "unittest_powershell"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        return
    fi

    POWERSHELL_VERSION=$(pwsh -v)
    log "powershell version: $POWERSHELL_VERSION"

    TESTS_SCRIPT="$PS1FIND_PATH/ps1find.tests.ps1"
    if [ ! -f "$TESTS_SCRIPT" ]
    then
        log_error "Test script not found: $TESTS_SCRIPT"
        return
    fi

    # run tests with powershell
    log "Unit-testing ps1find"
    log "pwsh $TESTS_SCRIPT"
    pwsh "$TESTS_SCRIPT"
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
    log "pytest"
    pytest

    # deactivate the virtualenv
    log "deactivate"
    deactivate

    cd -
}

unittest_ruby () {
    echo
    hdr "unittest_ruby"

    # ensure ruby3.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        log_error "You need to install ruby"
        return
    fi

    RUBY_VERSION="$(ruby -v 2>&1 | grep '^ruby 3')"
    if [ -z "$RUBY_VERSION" ]
    then
        log_error "A version of ruby >= 3.x is required"
        return
    fi

    log "ruby version: $RUBY_VERSION"

    # ensure bundler is installed
    # if [ -z "$(which bundle)" ]
    # then
    #     log_error "You need to install bundler: https://bundler.io/"
    #     return
    # fi

    # ensure rake is installed
    if [ -z "$(which rake)" ]
    then
        echo "You need to install rake"
        return
    fi

    cd "$RBFIND_PATH"

    # Run all tests via rake
    log "Unit-testing rbfind"
    log "bundle exec rake test"
    bundle exec rake test

    cd -
}

unittest_rust () {
    echo
    hdr "unittest_rust"

    # if rust is installed, display version
    if [ -n "$(which rustc)" ]
    then
        RUST_VERSION=$(rustc --version)
        log "rustc version: $RUST_VERSION"
    fi

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo"
        return
    fi

    CARGO_VERSION=$(cargo --version)
    log "cargo version: $CARGO_VERSION"

    cd "$RSFIND_PATH"

    # Run cargo test
    log "Unit-testing rsfind"
    log "cargo test --package rsfind --bin rsfind"
    cargo test --package rsfind --bin rsfind

    cd -
}

unittest_scala () {
    echo
    hdr "unittest_scala"

    # if scala is installed, display version
    if [ -n "$(which scala)" ]
    then
        SCALA_VERSION=$(scala -version 2>&1 | tail -n 1)
        log "$SCALA_VERSION"
    fi

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        echo "You need to install sbt"
        return
    fi

    SBT_VERSION=$(sbt --version | head -n 1)
    log "sbt version: $SBT_VERSION"

    cd "$SCALAFIND_PATH"

    # run tests via sbt
    log "Unit-testing scalafind"
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

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    cd "$SWIFTFIND_PATH"

    # run tests
    log "Unit-testing swiftfind"
    log "swift test"
    swift test

    cd -
}

unittest_typescript () {
    echo
    hdr "unittest_typescript"

    # if node is installed, display version
    if [ -n "$(which node)" ]
    then
        NODE_VERSION=$(node --version)
        log "node version: $NODE_VERSION"
    fi

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    cd "$TSFIND_PATH"

    # run tests
    log "Unit-testing tsfind"
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

    unittest_elixir

    unittest_fsharp

    unittest_go

    unittest_groovy

    unittest_haskell

    unittest_java

    unittest_javascript

    unittest_kotlin

    unittest_objc

    # unittest_ocaml

    unittest_perl

    unittest_php

    unittest_powershell

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
TEST_ALL=
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
            TEST_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "TEST_ALL: $TEST_ALL"
log "TARGET_LANGS: ${TARGET_LANGS[*]}"

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$TEST_ALL" ]
then
    unittest_all
    exit
fi

if [ ${#TARGET_LANGS[@]} == 0 ]
then
    usage
fi

for TARGET_LANG in ${TARGET_LANGS[*]}
do
    case $TARGET_LANG in
        # linux)
        #     unittest_linux
        #     ;;
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
        elixir | ex)
            unittest_elixir
            ;;
        fs | fsharp)
            unittest_fsharp
            ;;
        go)
            unittest_go
            ;;
        groovy)
            unittest_groovy
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
        # ocaml | ml)
        #     unittest_ocaml
        #     ;;
        perl | pl)
            unittest_perl
            ;;
        php)
            unittest_php
            ;;
        ps1 | powershell | pwsh)
            unittest_powershell
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
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done
