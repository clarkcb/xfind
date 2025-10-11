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

# Add failed builds to this array and report failed builds at the end
FAILED_BUILDS=()


########################################
# Utility Functions
########################################

usage () {
    echo -e "\nUsage: unittest.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}

print_failed_builds () {
    if [ ${#FAILED_BUILDS[@]} -gt 0 ]
    then
        log_error "Failed unit tests: ${FAILED_BUILDS[*]}"
    else
        log "All unit tests succeeded"
    fi
}

########################################
# Unit Test Functions
########################################

unittest_bashfind () {
    echo
    hdr "unittest_bashfind"

    # ensure bash is installed
    if [ -z "$(which bash)" ]
    then
        log_error "You need to install bash"
        FAILED_BUILDS+=("bashfind")
        return
    fi

    BASH_VERSION=$(bash --version | head -n 1)
    log "bash version: $BASH_VERSION"

    log "Unit-testing bashfind"
    log "$BASHFIND_PATH/test/bashfindtests.bash"
    $BASHFIND_PATH/test/bashfindtests.bash

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("bashfind")
    fi
}

unittest_cfind () {
    echo
    hdr "unittest_cfind"

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

                # check for success/failure
                if [ "$?" -ne 0 ]
                then
                    log_error "Tests failed"
                    FAILED_BUILDS+=("cfind")
                    return
                fi

            else
                log_error "cfind-tests not found: $CFIND_TEST_EXE"
                FAILED_BUILDS+=("cfind")
                return
            fi
        else
            log_error "cmake build directory not found: $CMAKE_BUILD_DIR"
            FAILED_BUILDS+=("cfind")
            return
        fi
    done

    log "Tests succeeded"
}

unittest_cljfind () {
    echo
    hdr "unittest_cljfind"

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
        log_error "You need to install lein"
        FAILED_BUILDS+=("cljfind")
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION=$(lein version)
    log "lein version: $LEIN_VERSION"

    cd "$CLJFIND_PATH"

    # Test with lein
    log "Unit-testing cljfind"
    log "lein test"
    lein test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("cljfind")
    fi

    cd -
}

unittest_cppfind () {
    echo
    hdr "unittest_cppfind"

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
                $CPPFIND_TEST_EXE

                if [ "$?" -eq 0 ]
                then
                    log "C++ unit test for $c succeeded"
                else
                    log_error "ERROR: cppfind unit tests for $c failed"
                    FAILED_BUILDS+=("cppfind")
                    return
                fi
            else
                log_error "cppfind-tests not found: $CPPFIND_TEST_EXE"
                FAILED_BUILDS+=("cppfind")
                return
            fi
        else
            log_error "cmake build directory not found: $CMAKE_BUILD_DIR"
            FAILED_BUILDS+=("cppfind")
            return
        fi
    done
}

unittest_csfind () {
    echo
    hdr "unittest_csfind"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("csfind")
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

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("csfind")
    fi
}

unittest_dartfind () {
    echo
    hdr "unittest_dartfind"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        FAILED_BUILDS+=("dartfind")
        return
    fi

    DART_VERSION=$(dart --version)
    log "$DART_VERSION"

    cd "$DARTFIND_PATH"

    log "Unit-testing dartfind"
    log "dart run test"
    dart run test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("dartfind")
    fi

    cd -
}

unittest_exfind () {
    echo
    hdr "unittest_exfind"

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
        FAILED_BUILDS+=("exfind")
        return
    fi

    MIX_VERSION=$(mix --version | grep Mix)
    log "mix version: $MIX_VERSION"

    cd "$EXFIND_PATH"

    # run tests
    log "Unit-testing exfind"
    log "mix test"
    mix test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("exfind")
    fi

    cd -
}

unittest_fsfind () {
    echo
    hdr "unittest_fsfind"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("fsfind")
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

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("fsfind")
    fi
}

unittest_gofind () {
    echo
    hdr "unittest_gofind"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        FAILED_BUILDS+=("gofind")
        return
    fi

    GO_VERSION=$(go version | sed 's/go version //')
    # GO_VERSION=$(go version | head -n 1 | cut -d ' ' -f 3)
    log "go version: $GO_VERSION"

    # Run the tests using go test
    log "Unit-testing gofind"
    cd "$GOFIND_PATH"

    log "go test --cover ./..."
    go test --cover ./...

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("gofind")
    fi

    cd -
}

unittest_groovyfind () {
    echo
    hdr "unittest_groovyfind"

    # if groovy is installed, display version
    if [ -n "$(which groovy)" ]
    then
        GROOVY_VERSION=$(groovy --version)
        log "$GROOVY_VERSION"
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
        FAILED_BUILDS+=("groovyfind")
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    GRADLE_GROOVY_VERSION=$(echo $GRADLE_OUTPUT | grep '^Groovy' | awk '{print $2}')
    log "Gradle Groovy version: $GRADLE_GROOVY_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    cd "$GROOVYFIND_PATH"

    # run tests via gradle
    log "Unit-testing groovyfind"
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("groovyfind")
    fi

    cd -
}

unittest_hsfind () {
    echo
    hdr "unittest_hsfind"

    # if ghc is installed, display version
    if [ -n "$(which ghc)" ]
    then
        GHC_VERSION=$(ghc --version)
        log "ghc version: $GHC_VERSION"
    fi

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        FAILED_BUILDS+=("hsfind")
        return
    fi

    STACK_VERSION=$(stack --version)
    log "stack version: $STACK_VERSION"

    cd "$HSFIND_PATH"
    
    # test with stack
    log "Unit-testing hsfind"
    log "stack test"
    stack test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("hsfind")
    fi

    cd -
}

unittest_javafind () {
    echo
    hdr "unittest_javafind"

    # if java is installed, display version
    if [ -n "$(which java)" ]
    then
        JAVA_VERSION=$(java -version 2>&1 | head -n 1)
        log "java version: $JAVA_VERSION"
    fi

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
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    KOTLIN_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $KOTLIN_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    # run tests via gradle
    log "Unit-testing javafind"
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("javafind")
    fi

    cd -
}

unittest_jsfind () {
    echo
    hdr "unittest_jsfind"

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
        FAILED_BUILDS+=("jsfind")
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    cd "$JSFIND_PATH"

    # run tests via npm
    log "Unit-testing jsfind"
    log "npm test"
    npm test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("jsfind")
    fi

    cd -
}

unittest_ktfind () {
    echo
    hdr "unittest_ktfind"

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
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    KOTLIN_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $KOTLIN_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    # run tests via gradle
    log "Unit-testing ktfind"
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("ktfind")
    fi

    cd -
}

unittest_objcfind () {
    echo
    hdr "unittest_objcfind"

    # TODO: copy resource files locally?
    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("objcfind")
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    cd "$OBJCFIND_PATH"

    log "Unit-testing objcfind"
    log "swift test"
    swift test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("objcfind")
    fi

    cd -
}

# unittest_mlfind () {
#     echo
#     hdr "unittest_mlfind"
#
#     cd "$MLFIND_PATH"
#
#     log "Unit-testing mlfind"
#     ./unittest.sh
#
#     cd -
# }

unittest_plfind () {
    echo
    hdr "unittest_plfind"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        log_error "You need to install perl"
        FAILED_BUILDS+=("plfind")
        return
    fi

    PERL_VERSION="$(perl -e 'print $^V' | grep '^v5')"
    if [ -z $PERL_VERSION ]
    then
        log_error "A 5.x version of perl is required"
        FAILED_BUILDS+=("plfind")
        return
    fi

    log "perl version: $PERL_VERSION"

    TESTS_PATH="$PLFIND_PATH/t"

    # run tests using Test::Simple
    log "Unit-testing plfind"
    FILES=$(find "$TESTS_PATH" -name "*_test.pl" | sort)
    for f in ${FILES[*]}
    do
        log "perl $f"
        IFS='' perl "$f" | tail -n +2 |
        while read line
        do
            echo "$line"
            if [[ ! "$line" =~ ^ok[[:space:]][0-9]+.+$ ]]
            then
                log_error "Tests failed"
                FAILED_BUILDS+=("plfind")
                return
            fi
        done
    done
}

unittest_phpfind () {
    echo
    hdr "unittest_phpfind"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        FAILED_BUILDS+=("phpfind")
        return
    fi

    PHP_VERSION=$(php -v | grep '^PHP [78]')
    if [ -z "$PHP_VERSION" ]
    then
        log_error "A version of PHP >= 7.x is required"
        FAILED_BUILDS+=("phpfind")
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
        log_error "You need to install phpunit (build.sh php)"
        FAILED_BUILDS+=("phpfind")
        return
    fi

    # run tests with phpunit
    log "Unit-testing phpfind"
    log "$PHPUNIT $TESTS_PATH"
    "$PHPUNIT" "$TESTS_PATH"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("phpfind")
    fi
}

unittest_ps1find () {
    echo
    hdr "unittest_ps1find"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        FAILED_BUILDS+=("ps1find")
        return
    fi

    POWERSHELL_VERSION=$(pwsh -v)
    log "powershell version: $POWERSHELL_VERSION"

    TESTS_SCRIPT="$PS1FIND_PATH/ps1find.tests.ps1"
    if [ ! -f "$TESTS_SCRIPT" ]
    then
        log_error "Test script not found: $TESTS_SCRIPT"
        FAILED_BUILDS+=("ps1find")
        return
    fi

    # run tests with powershell
    log "Unit-testing ps1find"
    log "pwsh $TESTS_SCRIPT"
    pwsh "$TESTS_SCRIPT"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("ps1find")
    fi
}

unittest_pyfind () {
    echo
    hdr "unittest_pyfind"

    TESTS_PATH="$PYFIND_PATH/tests"
    VENV_PATH="$PYFIND_PATH/venv"
    PYTHON="$VENV_PATH/bin/python"
    export PYTHONPATH="$PYTHON_PATH"

    if [ ! -d "$VENV_PATH" ]
    then
        log_error "venv path not found, you probably need to run the python build (./build.sh python)"
        FAILED_BUILDS+=("pyfind")
        return
    fi

    cd "$PYFIND_PATH"

    # activate the virtualenv
    log "source $VENV_PATH/bin/activate"
    source "$VENV_PATH/bin/activate"

    # Run the individual tests
    log "Unit-testing pyfind"
    log "pytest"
    pytest

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("pyfind")
    fi

    # deactivate the virtualenv
    log "deactivate"
    deactivate

    cd -
}

unittest_rbfind () {
    echo
    hdr "unittest_rbfind"

    # ensure ruby3.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        log_error "You need to install ruby"
        FAILED_BUILDS+=("rbfind")
        return
    fi

    RUBY_VERSION="$(ruby -v 2>&1 | grep '^ruby 3')"
    if [ -z "$RUBY_VERSION" ]
    then
        log_error "A version of ruby >= 3.x is required"
        FAILED_BUILDS+=("rbfind")
        return
    fi

    log "ruby version: $RUBY_VERSION"

    ensure bundler is installed
    if [ -z "$(which bundle)" ]
    then
        log_error "You need to install bundler: https://bundler.io/"
        FAILED_BUILDS+=("rbfind")
        return
    fi

    # ensure rake is installed
    if [ -z "$(which rake)" ]
    then
        log_error "You need to install rake"
        FAILED_BUILDS+=("rbfind")
        return
    fi

    cd "$RBFIND_PATH"

    # Run all tests via rake
    log "Unit-testing rbfind"
    log "bundle exec rake test"
    bundle exec rake test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("rbfind")
    fi

    cd -
}

unittest_rsfind () {
    echo
    hdr "unittest_rsfind"

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
        FAILED_BUILDS+=("rsfind")
        return
    fi

    CARGO_VERSION=$(cargo --version)
    log "cargo version: $CARGO_VERSION"

    cd "$RSFIND_PATH"

    # Run cargo test
    log "Unit-testing rsfind"
    log "cargo test --package rsfind --bin rsfind"
    cargo test --package rsfind --bin rsfind

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("rsfind")
    fi

    cd -
}

unittest_scalafind () {
    echo
    hdr "unittest_scalafind"

    # if scala is installed, display version
    if [ -n "$(which scala)" ]
    then
        SCALA_VERSION=$(scala -version 2>&1 | tail -n 1)
        log "$SCALA_VERSION"
    fi

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        log_error "You need to install sbt"
        FAILED_BUILDS+=("scalafind")
        return
    fi

    SBT_OUTPUT=$(sbt --version)

    SBT_PROJECT_VERSION=$(echo "$SBT_OUTPUT" | grep 'project')
    log "$SBT_PROJECT_VERSION"

    SBT_SCRIPT_VERSION=$(echo "$SBT_OUTPUT" | grep 'script')
    log "$SBT_SCRIPT_VERSION"

    JDK_VERSION=$(java -version  2>&1 | head -n 1)
    log "JDK version: $JDK_VERSION"

    cd "$SCALAFIND_PATH"

    # run tests via sbt
    log "Unit-testing scalafind"
    log "sbt test"
    sbt test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("scalafind")
    fi

    cd -
}

unittest_swiftfind () {
    echo
    hdr "unittest_swiftfind"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        FAILED_BUILDS+=("swiftfind")
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    cd "$SWIFTFIND_PATH"

    # run tests
    log "Unit-testing swiftfind"
    log "swift test"
    swift test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("swiftfind")
    fi

    cd -
}

unittest_tsfind () {
    echo
    hdr "unittest_tsfind"

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
        FAILED_BUILDS+=("tsfind")
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    cd "$TSFIND_PATH"

    # run tests
    log "Unit-testing tsfind"
    log "npm test"
    npm test

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Tests succeeded"
    else
        log_error "Tests failed"
        FAILED_BUILDS+=("tsfind")
    fi

    cd -
}

unittest_all () {
    hdr "unittest_all"

    unittest_bashfind

    unittest_cfind

    unittest_cljfind

    unittest_cppfind

    unittest_csfind

    unittest_dartfind

    unittest_exfind

    unittest_fsfind

    unittest_gofind

    unittest_groovyfind

    unittest_hsfind

    unittest_javafind

    unittest_jsfind

    unittest_ktfind

    unittest_objcfind

    # unittest_mlfind

    unittest_plfind

    unittest_phpfind

    unittest_ps1find

    unittest_pyfind

    unittest_rbfind

    unittest_rsfind

    unittest_scalafind

    unittest_swiftfind

    unittest_tsfind
}


########################################
# Unit-testing main
########################################
echo
hdr "xfind unittest script"
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
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$TEST_ALL" ]
then
    unittest_all
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
        bash)
            unittest_bashfind
            ;;
        c)
            unittest_cfind
            ;;
        clj | clojure)
            unittest_cljfind
            ;;
        cpp)
            unittest_cppfind
            ;;
        cs | csharp)
            unittest_csfind
            ;;
        dart)
            unittest_dartfind
            ;;
        elixir | ex)
            unittest_exfind
            ;;
        fs | fsharp)
            unittest_fsfind
            ;;
        go)
            unittest_gofind
            ;;
        groovy)
            unittest_groovyfind
            ;;
        haskell | hs)
            unittest_hsfind
            ;;
        java)
            unittest_javafind
            ;;
        javascript | js)
            unittest_jsfind
            ;;
        kotlin | kt)
            unittest_ktfind
            ;;
        objc)
            unittest_objcfind
            ;;
        # ocaml | ml)
        #     unittest_mlfind
        #     ;;
        perl | pl)
            unittest_plfind
            ;;
        php)
            unittest_phpfind
            ;;
        ps1 | powershell | pwsh)
            unittest_ps1find
            ;;
        py | python)
            unittest_pyfind
            ;;
        rb | ruby)
            unittest_rbfind
            ;;
        rs | rust)
            unittest_rsfind
            ;;
        scala)
            unittest_scalafind
            ;;
        swift)
            unittest_swiftfind
            ;;
        ts | typescript)
            unittest_tsfind
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_failed_builds
