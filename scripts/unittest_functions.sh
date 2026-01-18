#!/bin/bash
################################################################################
#
# unittest_functions.sh
#
# Run unit tests for xfind/xsearch implementations
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# source "$DIR/config.sh"
source "$DIR/common.sh"

# Global variable to hold last funtion exit code
UNITTEST_LASTEXITCODE=0

# Keep track of successful and failed tests
SUCCESSFUL_TESTS=()
FAILED_TESTS=()


########################################
# Utility Functions
########################################

print_test_results () {
    if [ ${#SUCCESSFUL_TESTS[@]} -gt 0 ]
    then
        log "Successful tests (${#SUCCESSFUL_TESTS[@]}): ${SUCCESSFUL_TESTS[*]}"
    else
        log_error "Successful tests: 0"
    fi
    if [ ${#FAILED_TESTS[@]} -gt 0 ]
    then
        log_error "Failed tests (${#FAILED_TESTS[@]}): ${FAILED_TESTS[*]}"
    else
        log "Failed tests: 0"
    fi
}

########################################
# Unit Test Functions
########################################

unittest_bash_version () {
    local base_path="$1"
    local bash_version_name="$2"

    log "language: bash"
    log "version: $bash_version_name"

    # ensure bash is installed
    if [ -z "$(which bash)" ]
    then
        log_error "You need to install bash"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    BASH_VERSION=$(bash --version | head -n 1)
    log "bash version: $BASH_VERSION"

    bash_version_path="$base_path/bash/$bash_version_name"
    log "bash_version_path: $bash_version_path"

    if [ ! -d "$bash_version_path" ]
    then
        log_error "Path not found: $bash_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    bash_version_test_script="$bash_version_path/test/${bash_version_name}tests.bash"

    if [ ! -f "$bash_version_test_script" ]
    then
        log_error "Test script not found: $bash_version_test_script"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "$bash_version_test_script"
    $bash_version_test_script

    UNITTEST_LASTEXITCODE=$?
}

unittest_c_version () {
    local base_path="$1"
    local c_version_name="$2"

    log "language: c"
    log "version: $c_version_name"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # cmake --version output looks like this: cmake version 3.30.2
    CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
    log "cmake version: $CMAKE_VERSION"

    c_version_path="$base_path/c/$c_version_name"
    log "c_version_path: $c_version_path"

    if [ ! -d "$c_version_path" ]
    then
        log_error "Path not found: $c_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    CONFIGURATIONS=(debug release)
    FOUND_TESTS=0
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$c_version_path/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]
        then
            C_VERSION_TEST_EXE=$CMAKE_BUILD_DIR/${c_version_name}-tests
            if [ -e "$C_VERSION_TEST_EXE" ]
            then
                FOUND_TESTS=1
                log "$C_VERSION_TEST_EXE"
                $C_VERSION_TEST_EXE
                # output=$(script -q tmpout $C_VERSION_TEST_EXE | tee /dev/tty)
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
                    UNITTEST_LASTEXITCODE=1
                    return
                fi

            else
                log_error "${c_version_name}-tests not found: $C_VERSION_TEST_EXE"
                UNITTEST_LASTEXITCODE=1
                return
            fi
        # else
        #     log_error "cmake build directory not found: $CMAKE_BUILD_DIR"
        #     return
        fi
    done

    if [ "$FOUND_TESTS" -eq 0 ]
    then
        log_error "No C unit tests found"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    UNITTEST_LASTEXITCODE=0
}

unittest_clojure_version () {
    local base_path="$1"
    local clj_version_name="$2"

    log "language: clojure"
    log "version: $clj_version_name"

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
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION=$(lein version)
    log "lein version: $LEIN_VERSION"

    clj_version_path="$base_path/clojure/$clj_version_name"
    log "clj_version_path: $clj_version_path"

    if [ ! -d "$clj_version_path" ]
    then
        log_error "Path not found: $clj_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$clj_version_path/resources"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $clj_version_path"
    cd "$clj_version_path"

    # Test with lein
    log "lein test"
    lein test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_cpp_version () {
    local base_path="$1"
    local cpp_version_name="$2"

    log "language: C++"
    log "version: $cpp_version_name"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # cmake --version output looks like this: cmake version 3.30.2
    CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
    log "cmake version: $CMAKE_VERSION"

    cpp_version_path="$base_path/cpp/$cpp_version_name"
    log "cpp_version_path: $cpp_version_path"

    if [ ! -d "$cpp_version_path" ]
    then
        log_error "Path not found: $cpp_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    CONFIGURATIONS=(debug release)
    FOUND_TESTS=0
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR=$cpp_version_path/cmake-build-$c
        if [ -d "$CMAKE_BUILD_DIR" ]
        then
            CPP_VERSION_TEST_EXE=$CMAKE_BUILD_DIR/${cpp_version_name}-tests
            if [ -e "$CPP_VERSION_TEST_EXE" ]
            then
                FOUND_TESTS=1
                log "$CPP_VERSION_TEST_EXE"
                $CPP_VERSION_TEST_EXE

                if [ "$?" -eq 0 ]
                then
                    log "C++ unit test for $c succeeded"
                else
                    log_error "ERROR: $cpp_version_name unit tests for $c failed"
                    UNITTEST_LASTEXITCODE=1
                    return
                fi
            else
                log_error "${cpp_version_name}-tests not found: $CPP_VERSION_TEST_EXE"
                UNITTEST_LASTEXITCODE=1
                return
            fi
        # else
        #     log_error "cmake build directory not found: $CMAKE_BUILD_DIR"
        #     return
        fi
    done

    if [ "$FOUND_TESTS" -eq 0 ]
    then
        log_error "No C++ unit tests found"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    UNITTEST_LASTEXITCODE=0
}

unittest_csharp_version () {
    local base_path="$1"
    local cs_version_name="$2"

    log "language: C#"
    log "version: $cs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    cs_version_path="$base_path/csharp/$cs_version_name"
    log "cs_version_path: $cs_version_path"

    if [ ! -d "$cs_version_path" ]
    then
        log_error "Path not found: $cs_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    PROJECT_PREFIX=""
    if [ "$cs_version_name" == 'csfind' ]
    then
        PROJECT_PREFIX='CsFind'
    elif [ "$cs_version_name" == 'cssearch' ]
    then
        PROJECT_PREFIX='CsSearch'
    else
        log_error "Unknown C# version name: $cs_version_name"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$cs_version_path/${PROJECT_PREFIX}Lib/Resources"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    TEST_RESOURCES_PATH="$cs_version_path/${PROJECT_PREFIX}Tests/Resources"
    TEST_RESOURCE_FILES=$(find "$TEST_RESOURCES_PATH" -type f)
    if [ -z "$TEST_RESOURCE_FILES" ]
    then
        log_error "Missing test resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # VERBOSITY=quiet
    VERBOSITY=minimal
    # VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "dotnet test $cs_version_path/${PROJECT_PREFIX}.sln --verbosity $VERBOSITY"
    dotnet test "$cs_version_path/${PROJECT_PREFIX}.sln" --verbosity $VERBOSITY

    UNITTEST_LASTEXITCODE=$?
}

unittest_dart_version () {
    local base_path="$1"
    local dart_version_name="$2"

    log "language: dart"
    log "version: $dart_version_name"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    DART_VERSION=$(dart --version)
    log "$DART_VERSION"

    dart_version_path="$base_path/dart/$dart_version_name"
    log "dart_version_path: $dart_version_path"

    if [ ! -d "$dart_version_path" ]
    then
        log_error "Path not found: $dart_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $dart_version_path"
    cd "$dart_version_path"

    log "dart run test"
    dart run test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_elixir_version () {
    local base_path="$1"
    local ex_version_name="$2"

    log "language: elixir"
    log "version: $ex_version_name"

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
        UNITTEST_LASTEXITCODE=1
        return
    fi

    MIX_VERSION=$(mix --version | grep Mix)
    log "mix version: $MIX_VERSION"

    ex_version_path="$base_path/elixir/$ex_version_name"
    log "ex_version_path: $ex_version_path"

    if [ ! -d "$ex_version_path" ]
    then
        log_error "Path not found: $ex_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $ex_version_path"
    cd "$ex_version_path"

    # run tests
    log "mix test"
    mix test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_fsharp_version () {
    local base_path="$1"
    local fs_version_name="$2"

    log "language: F#"
    log "version: $fs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    fs_version_path="$base_path/fsharp/$fs_version_name"
    log "fs_version_path: $fs_version_path"

    if [ ! -d "$fs_version_path" ]
    then
        log_error "Path not found: $fs_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    PROJECT_PREFIX=""
    if [ "$fs_version_name" == 'fsfind' ]
    then
        PROJECT_PREFIX='FsFind'
    elif [ "$fs_version_name" == 'fssearch' ]
    then
        PROJECT_PREFIX='FsSearch'
    else
        log_error "Unknown F# version name: $fs_version_name"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$fs_version_path/${PROJECT_PREFIX}Lib/Resources"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    TEST_RESOURCES_PATH="$fs_version_path/${PROJECT_PREFIX}Tests/Resources"
    TEST_RESOURCE_FILES=$(find "$TEST_RESOURCES_PATH" -type f)
    if [ -z "$TEST_RESOURCE_FILES" ]
    then
        log_error "Missing test resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # VERBOSITY=quiet
    VERBOSITY=minimal
    # VERBOSITY=normal
    # VERBOSITY=detailed

    # run dotnet test
    log "dotnet test $fs_version_path/${PROJECT_PREFIX}.sln --verbosity $VERBOSITY"
    dotnet test "$fs_version_path/${PROJECT_PREFIX}.sln" --verbosity $VERBOSITY

    UNITTEST_LASTEXITCODE=$?
}

unittest_go_version () {
    local base_path="$1"
    local go_version_name="$2"

    log "language: go"
    log "version: $go_version_name"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    GO_VERSION=$(go version | sed 's/go version //')
    # GO_VERSION=$(go version | head -n 1 | cut -d ' ' -f 3)
    log "go version: $GO_VERSION"

    go_version_path="$base_path/go/$go_version_name"
    log "go_version_path: $go_version_path"

    if [ ! -d "$go_version_path" ]
    then
        log_error "Path not found: $go_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $go_version_path"
    cd "$go_version_path"

    # Run the tests using go test
    log "go test --cover ./..."
    go test --cover ./...

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_groovy_version () {
    local base_path="$1"
    local groovy_version_name="$2"

    log "language: groovy"
    log "version: $groovy_version_name"

    # if groovy is installed, display version
    if [ -n "$(which groovy)" ]
    then
        GROOVY_VERSION=$(groovy --version)
        log "$GROOVY_VERSION"
    fi

    groovy_version_path="$base_path/groovy/$groovy_version_name"
    log "groovy_version_path: $groovy_version_path"

    if [ ! -d "$groovy_version_path" ]
    then
        log_error "Path not found: $groovy_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $groovy_version_path"
    cd "$groovy_version_path"

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
        cd -
        UNITTEST_LASTEXITCODE=1
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    GRADLE_GROOVY_VERSION=$(echo $GRADLE_OUTPUT | grep '^Groovy' | awk '{print $2}')
    log "Gradle Groovy version: $GRADLE_GROOVY_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    # run tests via gradle
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_haskell_version () {
    local base_path="$1"
    local hs_version_name="$2"

    log "language: haskell"
    log "version: $hs_version_name"

    # ensure ghc is installed
    if [ -z "$(which ghc)" ]
    then
        log_error "You need to install ghc"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    GHC_VERSION=$(ghc --version)
    log "ghc version: $GHC_VERSION"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    STACK_VERSION=$(stack --version)
    log "stack version: $STACK_VERSION"


    hs_version_path="$base_path/haskell/$hs_version_name"
    log "hs_version_path: $hs_version_path"

    if [ ! -d "$hs_version_path" ]
    then
        log_error "Path not found: $hs_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$hs_version_path/data"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $hs_version_path"
    cd "$hs_version_path"
    
    # test with stack
    log "stack test"
    stack test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_java_version () {
    local base_path="$1"
    local java_version_name="$2"

    log "language: java"
    log "version: $java_version_name"

    # ensure java is installed
    if [ -z "$(which java)" ]
    then
        log_error "You need to install java"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    JAVA_VERSION=$(java -version 2>&1 | head -n 1)
    log "java version: $JAVA_VERSION"

    java_version_path="$base_path/java/$java_version_name"
    log "java_version_path: $java_version_path"

    if [ ! -d "$java_version_path" ]
    then
        log_error "Path not found: $java_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$java_version_path/lib/src/main/resources"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    TEST_RESOURCES_PATH="$java_version_path/lib/src/test/resources"
    TEST_RESOURCE_FILES=$(find "$TEST_RESOURCES_PATH" -type f)
    if [ -z "$TEST_RESOURCE_FILES" ]
    then
        log_error "Missing test resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $java_version_path"
    cd "$java_version_path"

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
        cd -
        UNITTEST_LASTEXITCODE=1
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
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_javascript_version () {
    local base_path="$1"
    local js_version_name="$2"

    log "language: javascript"
    log "version: $js_version_name"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node.js"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    js_version_path="$base_path/javascript/$js_version_name"
    log "js_version_path: $js_version_path"

    if [ ! -d "$js_version_path" ]
    then
        log_error "Path not found: $js_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    if [ ! -d "$js_version_path/node_modules" ]
    then
        log_error "Missing node_modules directory, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $js_version_path"
    cd "$js_version_path"

    # run tests via npm
    log "npm test"
    npm test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_kotlin_version () {
    local base_path="$1"
    local kt_version_name="$2"

    log "language: kotlin"
    log "version: $kt_version_name"

    kt_version_path="$base_path/kotlin/$kt_version_name"
    log "kt_version_path: $kt_version_path"

    if [ ! -d "$kt_version_path" ]
    then
        log_error "Path not found: $kt_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$kt_version_path/lib/src/main/resources"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    TEST_RESOURCES_PATH="$kt_version_path/lib/src/test/resources"
    TEST_RESOURCE_FILES=$(find "$TEST_RESOURCES_PATH" -type f)
    if [ -z "$TEST_RESOURCE_FILES" ]
    then
        log_error "Missing test resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $kt_version_path"
    cd "$kt_version_path"

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
        cd -
        UNITTEST_LASTEXITCODE=1
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
    log "$GRADLE --warning-mode all test"
    "$GRADLE" --warning-mode all test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_objc_version () {
    local base_path="$1"
    local objc_version_name="$2"

    log "language: objc"
    log "version: $objc_version_name"

    # TODO: copy resource files locally?
    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    objc_version_path="$base_path/objc/$objc_version_name"
    log "objc_version_path: $objc_version_path"

    if [ ! -d "$objc_version_path" ]
    then
        log_error "Path not found: $objc_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $objc_version_path"
    cd "$objc_version_path"

    log "swift test"
    swift test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

# unittest_ml_version () {
#     echo
#     hdr "unittest_ml_version"
#
#     cd "$MLFIND_PATH"
#
#     log "Unit-testing mlfind"
#     ./unittest.sh
#
#     cd -
# }

unittest_perl_version () {
    local base_path="$1"
    local pl_version_name="$2"

    log "language: perl"
    log "version: $pl_version_name"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        log_error "You need to install perl"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    PERL_VERSION="$(perl -e 'print $^V' | grep '^v5')"
    if [ -z $PERL_VERSION ]
    then
        log_error "A 5.x version of perl is required"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "perl version: $PERL_VERSION"

    pl_version_path="$base_path/perl/$pl_version_name"
    log "pl_version_path: $pl_version_path"

    if [ ! -d "$pl_version_path" ]
    then
        log_error "Path not found: $pl_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    TESTS_PATH="$pl_version_path/t"

    # run tests using Test::Simple
    FILES=$(find "$TESTS_PATH" -name "*_test.pl" | sort)
    for f in ${FILES[*]}
    do
        log "perl $f"
        perl "$f"
        if [ "$?" -ne 0 ]
        then
            UNITTEST_LASTEXITCODE=1
            return
        fi
    done

    UNITTEST_LASTEXITCODE=0
}

unittest_php_version () {
    local base_path="$1"
    local php_version_name="$2"

    log "language: php"
    log "version: $php_version_name"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    PHP_VERSION=$(php -v | grep '^PHP [78]')
    if [ -z "$PHP_VERSION" ]
    then
        log_error "A version of PHP >= 7.x is required"
        UNITTEST_LASTEXITCODE=1
        return
    fi
    log "php version: $PHP_VERSION"

    # ensure composer is installed
    if [ -z "$(which composer)" ]
    then
        log_error "Need to install composer"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    COMPOSER_VERSION=$(composer --version 2>&1 | grep '^Composer')
    log "composer version: $COMPOSER_VERSION"

    php_version_path="$base_path/php/$php_version_name"
    log "php_version_path: $php_version_path"

    if [ ! -d "$php_version_path" ]
    then
        log_error "Path not found: $php_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$php_version_path/resources"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    PHPUNIT="$php_version_path/vendor/bin/phpunit"

    if [ ! -f "$PHPUNIT" ]
    then
        log_error "You need to install phpunit (build.sh php)"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    PHPUNIT_VERSION=$($PHPUNIT --version 2>&1 | grep '^PHPUnit')
    log "phpunit version: $PHPUNIT_VERSION"

    TESTS_PATH="$php_version_path/tests"

    # run tests with phpunit
    log "$PHPUNIT $TESTS_PATH"
    "$PHPUNIT" "$TESTS_PATH"

    UNITTEST_LASTEXITCODE=$?
}

unittest_powershell_version () {
    local base_path="$1"
    local ps1_version_name="$2"

    log "language: powershell"
    log "version: $ps1_version_name"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    POWERSHELL_VERSION=$(pwsh -v)
    log "powershell version: $POWERSHELL_VERSION"

    ps1_version_path="$base_path/powershell/$ps1_version_name"
    log "ps1_version_path: $ps1_version_path"

    if [ ! -d "$ps1_version_path" ]
    then
        log_error "Path not found: $ps1_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    TESTS_SCRIPT="$ps1_version_path/${ps1_version_name}.tests.ps1"
    if [ ! -f "$TESTS_SCRIPT" ]
    then
        log_error "Test script not found: $TESTS_SCRIPT"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # run tests with powershell
    log "pwsh $TESTS_SCRIPT"
    pwsh "$TESTS_SCRIPT"

    UNITTEST_LASTEXITCODE=$?
}

unittest_python_version () {
    local base_path="$1"
    local py_version_name="$2"

    log "language: python"
    log "version: $py_version_name"

    py_version_path="$base_path/python/$py_version_name"
    log "py_version_path: $py_version_path"

    if [ ! -d "$py_version_path" ]
    then
        log_error "Path not found: $py_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    echo "cd $py_version_path"
    cd "$py_version_path"

    USE_VENV=NO
    # TESTS_PATH="$py_version_path/tests"
    VENV_PATH="$py_version_path/venv"
    # PYTHON="$VENV_PATH/bin/python"
    PYTHON=
    export PYTHONPATH="$PYTHON_PATH"
    ACTIVE_VENV=NO
    ACTIVATED_VENV=NO

    if [ -d "$VENV_PATH" ]
    then
        USE_VENV=YES
        if [ -n "$VIRTUAL_ENV" ]
        then
            ACTIVE_VENV=YES
        else
            # activate the virtualenv
            log "source $VENV_PATH/bin/activate"
            source "$VENV_PATH/bin/activate"
            ACTIVATED_VENV=YES
        fi
    fi

    ERR=NO

    # ensure python3 is installed
    if [ -n "$(which python3)" ]
    then
        PYTHON="python3"
    else
        log_error "You need to install python3"
        ERR=YES
    fi

    if [ "$ERR" == "NO" ]
    then
        RESOURCES_PATH="$py_version_path/$py_version_name/data"
        RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
        if [ -z "$RESOURCE_FILES" ]
        then
            log_error "Missing resource files, need to run build first."
            ERR=YES
        fi
    fi

    if [ "$ERR" == "NO" ]
    then
        # Run the tests
        log "pytest"
        pytest

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Tests succeeded"
        else
            log_error "Tests failed"
            ERR=YES
        fi
    fi

    if [ "$ACTIVATED_VENV" == "YES" ]
    then
        # deactivate the virtualenv
        log "deactivate"
        deactivate
    fi

    cd -

    if [ "$ERR" == "YES" ]
    then
        UNITTEST_LASTEXITCODE=1
        return
    fi

    UNITTEST_LASTEXITCODE=0
}

unittest_ruby_version () {
    local base_path="$1"
    local rb_version_name="$2"

    log "language: ruby"
    log "version: $rb_version_name"

    # ensure ruby3.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        log_error "You need to install ruby"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RUBY_VERSION="$(ruby -v 2>&1 | grep '^ruby 3')"
    if [ -z "$RUBY_VERSION" ]
    then
        log_error "A version of ruby >= 3.x is required"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "ruby version: $RUBY_VERSION"

    # ensure bundler is installed
    if [ -z "$(which bundle)" ]
    then
        log_error "You need to install bundler: https://bundler.io/"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    BUNDLE_VERSION=$(bundle version 2>&1)
    log "bundle version: $BUNDLE_VERSION"

    # ensure rake is installed
    if [ -z "$(which rake)" ]
    then
        log_error "You need to install rake"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RAKE_VERSION=$(rake --version 2>&1)
    log "rake version: $RAKE_VERSION"

    rb_version_path="$base_path/ruby/$rb_version_name"
    log "rb_version_path: $rb_version_path"

    if [ ! -d "$rb_version_path" ]
    then
        log_error "Path not found: $rb_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$rb_version_path/data"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    TEST_RESOURCES_PATH="$rb_version_path/test/fixtures"
    TEST_RESOURCE_FILES=$(find "$TEST_RESOURCES_PATH" -type f)
    if [ -z "$TEST_RESOURCE_FILES" ]
    then
        log_error "Missing test resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $rb_version_path"
    cd "$rb_version_path"

    # Run all tests via rake
    log "bundle exec rake test"
    bundle exec rake test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_rust_version () {
    local base_path="$1"
    local rs_version_name="$2"

    log "language: rust"
    log "version: $rs_version_name"

    # ensure rust is installed
    if [ -z "$(which rustc)" ]
    then
        log_error "You need to install rust"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RUST_VERSION=$(rustc --version)
    log "rustc version: $RUST_VERSION"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    CARGO_VERSION=$(cargo --version)
    log "cargo version: $CARGO_VERSION"

    rs_version_path="$base_path/rust/$rs_version_name"
    log "rs_version_path: $rs_version_path"

    if [ ! -d "$rs_version_path" ]
    then
        log_error "Path not found: $rs_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $rs_version_path"
    cd "$rs_version_path"

    # Run cargo test
    log "cargo test --package $rs_version_name --bin $rs_version_name"
    cargo test --package "$rs_version_name" --bin "$rs_version_name"

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_scala_version () {
    local base_path="$1"
    local scala_version_name="$2"

    log "language: scala"
    log "version: $scala_version_name"

    # ensure scala is installed
    if [ -z "$(which scala)" ]
    then
        log_error "You need to install scala"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # scala --version output looks like this:
    # Scala code runner version: 1.4.3
    # Scala version (default): 3.7.4
    SCALA_VERSION=$(scala -version 2>&1 | tail -n 1 | cut -d ' ' -f 4)
    log "scala version: $SCALA_VERSION"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        log_error "You need to install sbt"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    SBT_OUTPUT=$(sbt --version)

    SBT_PROJECT_VERSION=$(echo "$SBT_OUTPUT" | grep 'project')
    log "$SBT_PROJECT_VERSION"

    SBT_SCRIPT_VERSION=$(echo "$SBT_OUTPUT" | grep 'script')
    log "$SBT_SCRIPT_VERSION"

    JDK_VERSION=$(java -version  2>&1 | head -n 1)
    log "JDK version: $JDK_VERSION"

    scala_version_path="$base_path/scala/$scala_version_name"
    log "scala_version_path: $scala_version_path"

    if [ ! -d "$scala_version_path" ]
    then
        log_error "Path not found: $scala_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$scala_version_path/src/main/resources"
    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    # TEST_RESOURCES_PATH="$scala_version_path/src/test/resources"

    log "cd $scala_version_path"
    cd "$scala_version_path"

    # run tests via sbt
    log "sbt test"
    sbt test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_swift_version () {
    local base_path="$1"
    local swift_version_name="$2"

    log "language: swift"
    log "version: $swift_version_name"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

    swift_version_path="$base_path/swift/$swift_version_name"
    log "swift_version_path: $swift_version_path"

    if [ ! -d "$swift_version_path" ]
    then
        log_error "Path not found: $swift_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $swift_version_path"
    cd "$swift_version_path"

    # run tests
    log "swift test"
    swift test

    UNITTEST_LASTEXITCODE=$?

    cd -
}

unittest_typescript_version () {
    local base_path="$1"
    local ts_version_name="$2"

    log "language: typescript"
    log "version: $ts_version_name"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node.js"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    ts_version_path="$base_path/typescript/$ts_version_name"
    log "ts_version_path: $ts_version_path"

    if [ ! -d "$ts_version_path" ]
    then
        log_error "Path not found: $ts_version_path"
        UNITTEST_LASTEXITCODE=1
        return
    fi

    if [ ! -d "$ts_version_path/node_modules" ]
    then
        log_error "Missing node_modules directory, need to run build first."
        UNITTEST_LASTEXITCODE=1
        return
    fi

    log "cd $ts_version_path"
    cd "$ts_version_path"

    # Disable web storage
    NODE_OPTIONS="--no-experimental-webstorage"

    # run tests
    log "npm test"
    # npm test
    NODE_OPTIONS="$NODE_OPTIONS" npm test
    
    UNITTEST_LASTEXITCODE=$?

    cd -
}
