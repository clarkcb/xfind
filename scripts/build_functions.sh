#!/bin/bash
################################################################################
#
# build_functions.sh
#
# Build functions for xfind or xsearch language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# source "$DIR/config.sh"
source "$DIR/common.sh"

# Global variable to hold last funtion exit code
BUILD_LASTEXITCODE=0

# Keep track of successful and failed builds
SUCCESSFUL_BUILDS=()
FAILED_BUILDS=()


########################################
# Utility Functions
########################################

# copy_json_resources
copy_json_resources () {
    local shared_path="$1"
    local resources_path="$2"
    log "cp $shared_path/*.json $resources_path/"
    cp "$shared_path"/*.json "$resources_path/"
}

# copy_test_resources
copy_test_resources () {
    local test_file_path="$1"
    local test_resources_path="$2"
    log "cp $test_file_path/testFile*.txt $test_resources_path/"
    cp "$test_file_path"/testFile*.txt "$test_resources_path/"
}

# add_to_bin
add_to_bin () {
    local bin_path="$1"
    local script_path="$2"
    local script_name=$(basename "$2")
    if [ ! -d "$bin_path" ]
    then
        log "Creating bin path"
        log "mkdir -p $bin_path"
        mkdir -p "$bin_path"
    fi

    cd "$bin_path"

    if [[ $script_name == *.sh || $script_name == *.bash || $script_name == *.ps1 ]]
    then
        script_name=${script_name%%.*}
    fi

    # echo "script_name: $script_name"
    # if [ -L "$script_name" ]
    # then
    #     log "rm $script_name"
    #     rm "$script_name"
    # fi

    log "ln -sf $script_path $script_name"
    ln -sf "$script_path" "$script_name"

    cd -
}

print_build_results () {
    if [ ${#SUCCESSFUL_BUILDS[@]} -gt 0 ]
    then
        log "Successful builds (${#SUCCESSFUL_BUILDS[@]}): ${SUCCESSFUL_BUILDS[*]}"
    else
        log_error "Successful builds: 0"
    fi
    if [ ${#FAILED_BUILDS[@]} -gt 0 ]
    then
        log_error "Failed builds (${#FAILED_BUILDS[@]}): ${FAILED_BUILDS[*]}"
    else
        log "Failed builds: 0"
    fi
}


########################################
# Build Functions
########################################

build_bash_version () {
    local base_path="$1"
    local bash_version_name="$2"

    log "language: bash"
    log "version: $bash_version_name"

    # ensure bash is installed
    if [ -z "$(which bash)" ]
    then
        log_error "You need to install bash"
        BUILD_LASTEXITCODE=1
        return
    fi

    BASH_VERSION=$(bash --version | head -n 1)
    log "bash version: $BASH_VERSION"

    bash_version_path="$base_path/bash/$bash_version_name"
    log "bash_version_path: $bash_version_path"

    if [ ! -d "$bash_version_path" ]
    then
        log_error "Path not found: $bash_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    script_path="$bash_version_path/bin/$bash_version_name.bash"

    # check that the expected executable script exists
    if [ ! -f "$script_path" ]
    then
        log_error "File not found: $script_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    bin_path="$base_path/bin"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?
}

build_c_version () {
    local base_path="$1"
    local c_version_name="$2"

    log "language: C"
    log "version: $c_version_name"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        BUILD_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $c_version_path"
    cd "$c_version_path"

    CONFIGURATIONS=()
    if [ -n "$DEBUG" ]
    then
        CONFIGURATIONS+=(debug)
    fi
    if [ -n "$RELEASE" ]
    then
        CONFIGURATIONS+=(release)
    fi

    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR="cmake-build-$c"
        CMAKE_BUILD_PATH="$c_version_path/$CMAKE_BUILD_DIR"

        if [ ! -d "$CMAKE_BUILD_PATH" ]
        then
            log "mkdir -p $CMAKE_BUILD_PATH"
            mkdir -p "$CMAKE_BUILD_PATH"

            log "cd $CMAKE_BUILD_PATH"
            cd "$CMAKE_BUILD_PATH"

            log "cmake -G \"Unix Makefiles\" -DCMAKE_BUILD_TYPE=$c .."
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            cd -
        fi

        if [ -d "$CMAKE_BUILD_PATH" ]
        then
            TARGETS=(clean "$c_version_name" "${c_version_name}app" "$c_version_name-tests")
            for t in ${TARGETS[*]}
            do
                log "cmake --build $CMAKE_BUILD_DIR --config $c --target $t"
                cmake --build "$CMAKE_BUILD_DIR" --config "$c" --target "$t"

                # check for success/failure
                if [ "$?" -eq 0 ]
                then
                    log "Build target $t succeeded"
                else
                    log_error "Build target $t failed"
                    cd -
                    BUILD_LASTEXITCODE=1
                    return
                fi
            done

            # now do the install
            # TODO: not sure if this belongs here, disabling for now
            # INSTALL_FILES=Y
            # if [ -n "$INSTALL_FILES" ]
            # then
            #     log "Installing $c_version_name files"
            #     log "cmake --install $CMAKE_BUILD_DIR --config $c --prefix /usr/local"
            #     cmake --install "$CMAKE_BUILD_DIR" --config "$c" --prefix /usr/local
            # fi
        fi
    done

    bin_path="$base_path/bin"
    script_path=""

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        script_path="$c_version_path/bin/$c_version_name.release.sh"
    else
        # add debug to bin
        script_path="$c_version_path/bin/$c_version_name.debug.sh"
    fi

    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_clojure_version () {
    local base_path="$1"
    local clj_version_name="$2"

    log "language: clojure"
    log "version: $clj_version_name"

    # ensure clojure is installed
    if [ -z "$(which clj)" ]
    then
        log_error "You need to install clojure"
        BUILD_LASTEXITCODE=1
        return
    fi

    # clj -version output looks like this: Clojure CLI version 1.11.4.1474
    # CLOJURE_VERSION=$(clj -version | head -n 1 | cut -d ' ' -f 3)
    CLOJURE_VERSION=$(clj -version 2>&1)
    log "clojure version: $CLOJURE_VERSION"

    # ensure leiningen is installed
    if [ -z "$(which lein)" ]
    then
        log_error "You need to install leiningen"
        BUILD_LASTEXITCODE=1
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION="$(lein version)"
    log "lein version: $LEIN_VERSION"

    clj_version_path="$base_path/clojure/$clj_version_name"
    log "clj_version_path: $clj_version_path"

    if [ ! -d "$clj_version_path" ]
    then
        log_error "clj version path not found: $clj_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    if [ ! -d "$SHARED_PATH" ]
    then
        log_error "Path not found: $SHARED_PATH"
        BUILD_LASTEXITCODE=1
        return
    fi
    RESOURCES_PATH="$clj_version_path/resources"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    log "cd $clj_version_path"
    cd "$clj_version_path"

    RESOURCE_FILES=$(find "$RESOURCES_PATH" -name "*.json")
    if [ -z "$RESOURCE_FILES" ]
    then
        log_error "Missing resource files, need to run build first."
        UNITTEST_LASTEXITCODE=1
        cd -
        return
    fi

    # Create uberjar with lein
    log "lein clean"
    lein clean

    # install to local maven repository
    log "lein install"
    lein install

    # create uberjar
    log "lein uberjar"
    lein uberjar

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$clj_version_path/bin/$clj_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_cpp_version () {
    local base_path="$1"
    local cpp_version_name="$2"

    log "language: C++"
    log "version: $cpp_version_name"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        BUILD_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $cpp_version_path"
    cd "$cpp_version_path"

    # CMAKE_CXX_FLAGS="-W -Wall -Werror"
    CMAKE_CXX_FLAGS="-W -Wall -Werror -Wextra -Wshadow -Wnon-virtual-dtor -pedantic"

    # Add AddressSanitizer
    # CMAKE_CXX_FLAGS="$CMAKE_CXX_FLAGS -fsanitize=address -fno-omit-frame-pointer"

    CONFIGURATIONS=()
    if [ -n "$DEBUG" ]
    then
        CONFIGURATIONS+=(debug)
    fi
    if [ -n "$RELEASE" ]
    then
        CONFIGURATIONS+=(release)
    fi

    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR="cmake-build-$c"
        CMAKE_BUILD_PATH="$cpp_version_path/$CMAKE_BUILD_DIR"

        if [ ! -d "$CMAKE_BUILD_PATH" ]
        then
            log "mkdir -p $CMAKE_BUILD_PATH"
            mkdir -p "$CMAKE_BUILD_PATH"

            log "cd $CMAKE_BUILD_PATH"
            cd "$CMAKE_BUILD_PATH"

            log "cmake -G \"Unix Makefiles\" -DCMAKE_BUILD_TYPE=$c .."
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            cd -
        fi

        if [ -d "$CMAKE_BUILD_PATH" ]
        then
            TARGETS=(clean $cpp_version_name ${cpp_version_name}app $cpp_version_name-tests)
            for t in ${TARGETS[*]}
            do
                log "cmake --build $CMAKE_BUILD_DIR --config $c --target $t -- $CMAKE_CXX_FLAGS"
                cmake --build "$CMAKE_BUILD_DIR" --config "$c" --target "$t" -- "$CMAKE_CXX_FLAGS"

                # check for success/failure
                if [ "$?" -eq 0 ]
                then
                    log "Build target $t succeeded"
                else
                    log_error "Build target $t failed"
                    cd -
                    BUILD_LASTEXITCODE=1
                    return
                fi
            done

            # now do the install
            # TODO: not sure if this belongs here, disabling for now
            # INSTALL_FILES=Y
            # if [ -n "$INSTALL_FILES" ]
            # then
            #     log "Installing $cpp_version_name files"
            #     log "cmake --install $CMAKE_BUILD_DIR --config $c --prefix /usr/local"
            #     cmake --install "$CMAKE_BUILD_DIR" --config "$c" --prefix /usr/local
            # fi
        fi
    done

    bin_path="$base_path/bin"
    script_path=""

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        script_path="$cpp_version_path/bin/$cpp_version_name.release.sh"
    else
        # add debug to bin
        script_path="$cpp_version_path/bin/$cpp_version_name.debug.sh"
    fi

    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_csharp_version () {
    local base_path="$1"
    local cs_version_name="$2"

    log "language: C#"
    log "version: $cs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        BUILD_LASTEXITCODE=1
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    cs_version_path="$base_path/csharp/$cs_version_name"
    log "cs_version_path: $cs_version_path"

    if [ ! -d "$cs_version_path" ]
    then
        log_error "Path not found: $cs_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    # TODO: get dir names with cs_version_name but different casing
    # For now, we're going to match on the two known version names

    PROJECT_PREFIX=""
    if [ "$cs_version_name" == 'csfind' ]
    then
        PROJECT_PREFIX='CsFind'
    elif [ "$cs_version_name" == 'cssearch' ]
    then
        PROJECT_PREFIX='CsSearch'
    else
        log_error "Unknown C# version name: $cs_version_name"
        BUILD_LASTEXITCODE=1
        return
    fi

    SOLUTION_PATH="$cs_version_path/${PROJECT_PREFIX}.sln"
    RESOURCES_PATH="$cs_version_path/${PROJECT_PREFIX}Lib/Resources"
    TEST_RESOURCES_PATH="$cs_version_path/${PROJECT_PREFIX}Tests/Resources"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    TEST_FILE_PATH=$SHARED_PATH/testFiles
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_FILE_PATH" "$TEST_RESOURCES_PATH"

    CONFIGURATIONS=()
    if [ -n "$DEBUG" ]
    then
        CONFIGURATIONS+=(Debug)
    fi
    if [ -n "$RELEASE" ]
    then
        CONFIGURATIONS+=(Release)
    fi

    # run dotnet build for selected configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "dotnet build $SOLUTION_PATH --configuration $c"
        dotnet build "$SOLUTION_PATH" --configuration "$c"

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            BUILD_LASTEXITCODE=1
            return
        fi
    done

    bin_path="$base_path/bin"
    script_path=""

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        script_path="$cs_version_path/bin/$cs_version_name.release.sh"
    else
        # add debug to bin
        script_path="$cs_version_path/bin/$cs_version_name.debug.sh"
    fi

    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?
}

build_dart_version () {
    local base_path="$1"
    local dart_version_name="$2"

    log "language: dart"
    log "version: $dart_version_name"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        BUILD_LASTEXITCODE=1
        return
    fi

    DART_VERSION=$(dart --version)
    log "$DART_VERSION"

    dart_version_path="$base_path/dart/$dart_version_name"
    log "dart_version_path: $dart_version_path"

    if [ ! -d "$dart_version_path" ]
    then
        log_error "Path not found: $dart_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $dart_version_path"
    cd "$dart_version_path"

    # RESOURCES_PATH="$dart_version_path/lib/data"

    # TODO: move resources to local location, for now read relative to XFIND_PATH
    # mkdir -p "$RESOURCES_PATH"
    # copy_json_resources "$RESOURCES_PATH"

    if [ ! -f "$dart_version_path/.dart_tool/package_config.json" ] && [ ! -f "$dart_version_path/.packages" ]
    then
        log "dart pub get"
        dart pub get
    else
        log "dart pub upgrade"
        dart pub upgrade
    fi

    log "Compiling $dart_version_name"
    dart_script="$dart_version_path/bin/$dart_version_name.dart"
    log "dart compile exe $dart_script"
    dart compile exe "$dart_script"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$dart_version_path/bin/$dart_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_elixir_version () {
    local base_path="$1"
    local ex_version_name="$2"

    log "language: elixir"
    log "version: $ex_version_name"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        BUILD_LASTEXITCODE=1
        return
    fi

    ELIXIR_VERSION=$(elixir --version | grep Elixir)
    log "elixir version: $ELIXIR_VERSION"

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        BUILD_LASTEXITCODE=1
        return
    fi

    MIX_VERSION=$(mix --version | grep Mix)
    log "mix version: $MIX_VERSION"

    ex_version_path="$base_path/elixir/$ex_version_name"
    log "ex_version_path: $ex_version_path"

    if [ ! -d "$ex_version_path" ]
    then
        log_error "Path not found: $ex_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $ex_version_path"
    cd "$ex_version_path"

    log "Getting $ex_version_name dependencies"
    log "mix deps.get"
    mix deps.get

    log "Compiling $ex_version_name"
    log "mix compile"
    mix compile

    log "Creating $ex_version_name executable"
    log "mix escript.build"
    mix escript.build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$ex_version_path/bin/$ex_version_name"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_fsharp_version () {
    local base_path="$1"
    local fs_version_name="$2"

    log "language: F#"
    log "version: $fs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        BUILD_LASTEXITCODE=1
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    fs_version_path="$base_path/fsharp/$fs_version_name"
    log "fs_version_path: $fs_version_path"

    if [ ! -d "$fs_version_path" ]
    then
        log_error "Path not found: $fs_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    # TODO: get dir names with fs_version_name but different casing
    # For now, we're going to match on the two known version names

    PROJECT_PREFIX=""
    if [ "$fs_version_name" == 'fsfind' ]
    then
        PROJECT_PREFIX='FsFind'
    elif [ "$fs_version_name" == 'fssearch' ]
    then
        PROJECT_PREFIX='FsSearch'
    else
        log_error "Unknown F# version name: $fs_version_name"
        BUILD_LASTEXITCODE=1
        return
    fi

    SOLUTION_PATH="$fs_version_path/${PROJECT_PREFIX}.sln"
    RESOURCES_PATH="$fs_version_path/${PROJECT_PREFIX}Lib/Resources"
    TEST_RESOURCES_PATH="$fs_version_path/${PROJECT_PREFIX}Tests/Resources"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    TEST_FILE_PATH=$SHARED_PATH/testFiles
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_FILE_PATH" "$TEST_RESOURCES_PATH"

    CONFIGURATIONS=()
    if [ -n "$DEBUG" ]
    then
        CONFIGURATIONS+=(Debug)
    fi
    if [ -n "$RELEASE" ]
    then
        CONFIGURATIONS+=(Release)
    fi

    # run dotnet build for selected configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "dotnet build $SOLUTION_PATH --configuration $c"
        dotnet build "$SOLUTION_PATH" --configuration "$c"

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            BUILD_LASTEXITCODE=1
            return
        fi
    done

    bin_path="$base_path/bin"
    script_path=""

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        script_path="$fs_version_path/bin/$fs_version_name.release.sh"
    else
        # add debug to bin
        script_path="$fs_version_path/bin/$fs_version_name.debug.sh"
    fi

    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?
}

build_go_version () {
    local base_path="$1"
    local go_version_name="$2"

    log "language: go"
    log "version: $go_version_name"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        BUILD_LASTEXITCODE=1
        return
    fi

    GO_VERSION=$(go version | sed 's/go version //')
    # GO_VERSION=$(go version | head -n 1 | cut -d ' ' -f 3)
    log "go version: $GO_VERSION"

    # build the code to generate the dynamic code for gofind
    #echo "go install elocale.com/clarkcb/gofindcodegen/gengofindcode"
    #go install elocale.com/clarkcb/gofindcodegen/gengofindcode

    # run it to generate the dynamic gofind code
    #log "Running gengofindcode"
    #log "gengofindcode"
    #gengofindcode

    go_version_path="$base_path/go/$go_version_name"
    log "go_version_path: $go_version_path"

    if [ ! -d "$go_version_path" ]
    then
        log_error "Path not found: $go_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $go_version_path"
    cd "$go_version_path"

    # go fmt the gofind source (for auto-generated code)
    log "Auto-formatting $go_version_name"
    log "go fmt ./..."
    go fmt ./...

    # check for success/failure
    if [ "$?" -ne 0 ]
    then
        log_error "Auto-formatting failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # create the bin dir if it doesn't already exist
    BIN_PATH="$base_path/bin"
    if [ ! -d "$BIN_PATH" ]
    then
        mkdir -p "$BIN_PATH"
    fi

    # if GOBIN not defined, set to BIN_PATH
    if [ -z "$GOBIN" ]
    then
        GOBIN="$BIN_PATH"
    fi

    # now build/install go version
    log "GOBIN=$GOBIN go install ./..."
    GOBIN="$GOBIN" go install ./...

    BUILD_LASTEXITCODE=$?

    cd -
}

build_groovy_version () {
    local base_path="$1"
    local groovy_version_name="$2"

    log "language: groovy"
    log "version: $groovy_version_name"

    # ensure groovy is installed
    if [ -z "$(which groovy)" ]
    then
        log_error "You need to install groovy"
        BUILD_LASTEXITCODE=1
        return
    fi

    GROOVY_VERSION=$(groovy --version)
    log "$GROOVY_VERSION"

    groovy_version_path="$base_path/groovy/$groovy_version_name"
    log "groovy_version_path: $groovy_version_path"

    if [ ! -d "$groovy_version_path" ]
    then
        log_error "Path not found: $groovy_version_path"
        BUILD_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)
    # ------------------------------------------------------------
    # Gradle 8.10.2
    # ------------------------------------------------------------

    # Build time:    2024-09-23 21:28:39 UTC
    # Revision:      415adb9e06a516c44b391edff552fd42139443f7

    # Kotlin:        1.9.24
    # Groovy:        3.0.22
    # Ant:           Apache Ant(TM) version 1.10.14 compiled on August 16 2023
    # Launcher JVM:  11.0.24 (Homebrew 11.0.24+0)
    # Daemon JVM:    /usr/local/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home (no JDK specified, using current Java home)
    # OS:            Mac OS X 14.6.1 x86_64

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    GRADLE_GROOVY_VERSION=$(echo $GRADLE_OUTPUT | grep '^Groovy' | awk '{print $2}')
    log "Gradle Groovy version: $GRADLE_GROOVY_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    RESOURCES_PATH="$groovy_version_path/lib/src/main/resources"
    TEST_RESOURCES_PATH="$groovy_version_path/lib/src/test/resources"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    TEST_FILE_PATH=$SHARED_PATH/testFiles
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_FILE_PATH" "$TEST_RESOURCES_PATH"

    # run a gradle build
    # log "gradle --warning-mode all clean jar publishToMavenLocal"
    # gradle --warning-mode all clean jar publishToMavenLocal
    # GRADLE_ARGS="--info --warning-mode all"
    GRADLE_ARGS="--warning-mode all"
    GRADLE_TASKS=(clean :lib:jar :lib:publishToMavenLocal :app:jar)
    for t in ${GRADLE_TASKS[*]}
    do
        log "$GRADLE $GRADLE_ARGS $t"
        "$GRADLE" --warning-mode all $t
    done

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$groovy_version_path/bin/$groovy_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_haskell_version () {
    local base_path="$1"
    local hs_version_name="$2"

    log "language: haskell"
    log "version: $hs_version_name"

    # ensure ghc is installed
    if [ -z "$(which ghc)" ]
    then
        log_error "You need to install ghc"
        BUILD_LASTEXITCODE=1
        return
    fi

    GHC_VERSION=$(ghc --version)
    log "ghc version: $GHC_VERSION"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        BUILD_LASTEXITCODE=1
        return
    fi

    STACK_VERSION=$(stack --version)
    log "stack version: $STACK_VERSION"

    # set the default stack settings, e.g. use system ghc
    STACK_DIR=$HOME/.stack
    if [ ! -d "$STACK_DIR" ]
    then
        mkdir -p "$STACK_DIR"
    fi
    if [ ! -f "$STACK_DIR/config.yaml" ]
    then
        touch "$STACK_DIR/config.yaml"
    fi
    INSTALL_GHC=$(grep '^install-ghc:' "$STACK_DIR"/config.yaml)
    if [ -z "$INSTALL_GHC" ]
    then
        echo 'install-ghc: false' >> "$STACK_DIR/config.yaml"
    fi
    SYSTEM_GHC=$(grep '^system-ghc:' "$STACK_DIR"/config.yaml)
    if [ -z "$SYSTEM_GHC" ]
    then
        echo 'system-ghc: true' >> "$STACK_DIR/config.yaml"
    fi

    hs_version_path="$base_path/haskell/$hs_version_name"
    log "hs_version_path: $hs_version_path"

    if [ ! -d "$hs_version_path" ]
    then
        log_error "Path not found: $hs_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    RESOURCES_PATH="$hs_version_path/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    cd "$hs_version_path/"

    # build with stack (via make)
    log "stack setup"
    make setup

    log "stack build"
    make build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    log "stack install --local-bin-path $base_path/bin"
    stack install --local-bin-path "$base_path/bin"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_java_version () {
    local base_path="$1"
    local java_version_name="$2"

    log "language: java"
    log "version: $java_version_name"

    # ensure java is installed
    if [ -z "$(which java)" ]
    then
        log_error "You need to install java"
        BUILD_LASTEXITCODE=1
        return
    fi

    JAVA_VERSION=$(java -version 2>&1 | head -n 1)
    log "java version: $JAVA_VERSION"

    java_version_path="$base_path/java/$java_version_name"
    log "java_version_path: $java_version_path"

    if [ ! -d "$java_version_path" ]
    then
        log_error "Path not found: $java_version_path"
        BUILD_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)
    # ------------------------------------------------------------
    # Gradle 8.10.2
    # ------------------------------------------------------------

    # Build time:    2024-09-23 21:28:39 UTC
    # Revision:      415adb9e06a516c44b391edff552fd42139443f7

    # Kotlin:        1.9.24
    # Groovy:        3.0.22
    # Ant:           Apache Ant(TM) version 1.10.14 compiled on August 16 2023
    # Launcher JVM:  11.0.24 (Homebrew 11.0.24+0)
    # Daemon JVM:    /usr/local/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home (no JDK specified, using current Java home)
    # OS:            Mac OS X 14.6.1 x86_64

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    KOTLIN_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $KOTLIN_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    RESOURCES_PATH="$java_version_path/lib/src/main/resources"
    TEST_RESOURCES_PATH="$java_version_path/lib/src/test/resources"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    TEST_FILE_PATH=$SHARED_PATH/testFiles
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_FILE_PATH" "$TEST_RESOURCES_PATH"

    # run a gradle clean jar build
    # log "gradle --warning-mode all clean jar publishToMavenLocal"
    # gradle --warning-mode all clean jar publishToMavenLocal
    # GRADLE_ARGS="--info --warning-mode all"
    GRADLE_ARGS="--warning-mode all"
    GRADLE_TASKS=(clean :lib:jar :lib:publishToMavenLocal :app:jar)
    for t in ${GRADLE_TASKS[*]}
    do
        log "$GRADLE $GRADLE_ARGS $t"
        "$GRADLE" --warning-mode all $t
    done

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$java_version_path/bin/$java_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_javascript_version () {
    local base_path="$1"
    local js_version_name="$2"

    log "language: javascript"
    log "version: $js_version_name"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node.js"
        BUILD_LASTEXITCODE=1
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        BUILD_LASTEXITCODE=1
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    js_version_path="$base_path/javascript/$js_version_name"
    log "js_version_path: $js_version_path"

    if [ ! -d "$js_version_path" ]
    then
        log_error "Path not found: $js_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    RESOURCES_PATH="$js_version_path/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    log "cd $js_version_path"
    cd "$js_version_path"

    # run npm install and build
    log "npm install"
    npm install

    log "npm run build"
    npm run build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$js_version_path/bin/$js_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_kotlin_version () {
    local base_path="$1"
    local kt_version_name="$2"

    log "language: kotlin"
    log "version: $kt_version_name"

    kt_version_path="$base_path/kotlin/$kt_version_name"
    log "kt_version_path: $kt_version_path"

    if [ ! -d "$kt_version_path" ]
    then
        log_error "Path not found: $kt_version_path"
        BUILD_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)
    # echo "$GRADLE_OUTPUT"

    # ------------------------------------------------------------
    # Gradle 8.10.2
    # ------------------------------------------------------------

    # Build time:    2024-09-23 21:28:39 UTC
    # Revision:      415adb9e06a516c44b391edff552fd42139443f7

    # Kotlin:        1.9.24
    # Groovy:        3.0.22
    # Ant:           Apache Ant(TM) version 1.10.14 compiled on August 16 2023
    # Launcher JVM:  11.0.24 (Homebrew 11.0.24+0)
    # Daemon JVM:    /usr/local/Cellar/openjdk@11/11.0.24/libexec/openjdk.jdk/Contents/Home (no JDK specified, using current Java home)
    # OS:            Mac OS X 14.6.1 x86_64

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    KOTLIN_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $KOTLIN_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    RESOURCES_PATH="$kt_version_path/lib/src/main/resources"
    TEST_RESOURCES_PATH="$kt_version_path/lib/src/test/resources"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    TEST_FILE_PATH=$SHARED_PATH/testFiles
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_FILE_PATH" "$TEST_RESOURCES_PATH"

    # run a gradle build
    # log "gradle --warning-mode all clean jar publishToMavenLocal"
    # gradle --warning-mode all clean jar publishToMavenLocal
    # GRADLE_ARGS="--info --warning-mode all"
    GRADLE_ARGS="--warning-mode all"
    GRADLE_TASKS=(clean :lib:jar :lib:publishToMavenLocal :app:jar)
    for t in ${GRADLE_TASKS[*]}
    do
        log "$GRADLE $GRADLE_ARGS $t"
        "$GRADLE" --warning-mode all $t
    done

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$kt_version_path/bin/$kt_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_ocaml_version () {
    local base_path="$1"
    local ml_version_name="$2"

    log "language: ocaml"
    log "version: $ml_version_name"

    ml_version_path="$base_path/ocaml/$ml_version_name"
    log "ml_version_path: $ml_version_path"

    cd "$MLFIND_PATH"
    ./build.sh
    # if [ -L ~/bin/mlfind ]
    # then
    #     rm ~/bin/mlfind
    # fi
    ln -sf "$MLFIND_PATH/_build/src/mlfind.native" ~/bin/mlfind

    cd -
}

build_objc_version () {
    local base_path="$1"
    local objc_version_name="$2"

    log "language: objc"
    log "version: $objc_version_name"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        BUILD_LASTEXITCODE=1
        return
    fi

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    SWIFT_VERSION=$(swift --version 2>&1 | grep 'Apple Swift' | cut -d ' ' -f 7)
    log "swift version: Apple Swift version $SWIFT_VERSION"

    objc_version_path="$base_path/objc/$objc_version_name"
    log "objc_version_path: $objc_version_path"

    if [ ! -d "$objc_version_path" ]
    then
        log_error "Path not found: $objc_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $objc_version_path"
    cd "$objc_version_path"

    # run swift build
    if [ -n "$DEBUG" ]
    then
        log "swift build"
        swift build

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            cd -
            BUILD_LASTEXITCODE=1
            return
        fi
    fi

    bin_path="$base_path/bin"
    script_path=""

    if [ -n "$RELEASE" ]
    then
        log "swift build --configuration release"
        swift build --configuration release

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            cd -
            BUILD_LASTEXITCODE=1
            return
        fi

        # add release to bin
        script_path="$objc_version_path/bin/$objc_version_name.release.sh"
    else
        # add debug to bin
        script_path="$objc_version_path/bin/$objc_version_name.debug.sh"
    fi

    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_perl_version () {
    local base_path="$1"
    local pl_version_name="$2"

    log "language: perl"
    log "version: $pl_version_name"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        log_error "You need to install perl"
        BUILD_LASTEXITCODE=1
        return
    fi

    PERL_VERSION="$(perl -e 'print $^V' | grep '^v5')"
    if [ -z $PERL_VERSION ]
    then
        log_error "A 5.x version of perl is required"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "perl version: $PERL_VERSION"

    # # ensure carton is installed
    # if [ -z "$(which carton)" ]
    # then
    #     log_error "Need to install carton"
    #     BUILD_LASTEXITCODE=1
    #     return
    # fi

    # CARTON_VERSION=$(carton --version 2>&1)
    # log "carton version: $CARTON_VERSION"

    pl_version_path="$base_path/perl/$pl_version_name"
    log "pl_version_path: $pl_version_path"

    if [ ! -d "$pl_version_path" ]
    then
        log_error "Path not found: $pl_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    RESOURCES_PATH="$pl_version_path/share"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # check for success/failure
    if [ "$?" -ne 0 ]
    then
        log_error "Failed copying json resources"
        BUILD_LASTEXITCODE=1
        return
    fi

    # install perl dependencies via carton
    log "cd $pl_version_path"
    cd "$pl_version_path"

    # log "carton install"
    # carton install

    # # check for success/failure
    # if [ "$?" -ne 0 ]
    # then
    #     log_error "Failed carton install"
    #     BUILD_LASTEXITCODE=1
    #     return
    # fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$pl_version_path/bin/$pl_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?
}

build_php_version () {
    local base_path="$1"
    local php_version_name="$2"

    log "language: php"
    log "version: $php_version_name"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        BUILD_LASTEXITCODE=1
        return
    fi

    # PHP_VERSION=$(php -r "echo phpversion();")
    PHP_VERSION=$(php -v | grep '^PHP [78]')
    if [ -z "$PHP_VERSION" ]
    then
        log_error "A version of PHP >= 7.x is required"
        BUILD_LASTEXITCODE=1
        return
    fi
    log "php version: $PHP_VERSION"

    # ensure composer is installed
    if [ -z "$(which composer)" ]
    then
        log_error "Need to install composer"
        BUILD_LASTEXITCODE=1
        return
    fi

    COMPOSER_VERSION=$(composer --version 2>&1 | grep '^Composer')
    log "composer version: $COMPOSER_VERSION"

    php_version_path="$base_path/php/$php_version_name"
    log "php_version_path: $php_version_path"

    if [ ! -d "$php_version_path" ]
    then
        log_error "Path not found: $php_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    CONFIG_PATH="$php_version_path/config"
    RESOURCES_PATH="$php_version_path/resources"
    SHARED_PATH="$base_path/shared"

    # copy the shared config json file to the local config location
    if [ -f "$SHARED_PATH/config.json" ]
    then
        mkdir -p "$CONFIG_PATH"
        log "cp $SHARED_PATH/config.json $CONFIG_PATH/"
        cp "$SHARED_PATH/config.json" "$CONFIG_PATH/"
    fi

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    log "cp $SHARED_PATH/*.json $RESOURCES_PATH/"
    cp "$SHARED_PATH"/*.json "$RESOURCES_PATH/"

    log "cd $php_version_path"
    cd "$php_version_path"

    # run a composer build
    if [ -d "$php_version_path/vendor" ]
    then
        log "composer update"
        composer update
    else
        log "composer install"
        composer install
    fi

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$php_version_path/bin/$php_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_powershell_version () {
    local base_path="$1"
    local ps1_version_name="$2"

    log "language: powershell"
    log "version: $ps1_version_name"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        BUILD_LASTEXITCODE=1
        return
    fi

    POWERSHELL_VERSION=$(pwsh -v)
    log "powershell version: $POWERSHELL_VERSION"

    ps1_version_path="$base_path/powershell/$ps1_version_name"
    log "ps1_version_path: $ps1_version_path"

    if [ ! -d "$ps1_version_path" ]
    then
        log_error "Path not found: $ps1_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    PROJECT_PREFIX=""
    if [ "$ps1_version_name" == 'ps1find' ]
    then
        PROJECT_PREFIX='Ps1Find'
    elif [ "$ps1_version_name" == 'ps1search' ]
    then
        PROJECT_PREFIX='Ps1Search'
    else
        log_error "Unknown powershell version name: $ps1_version_name"
        BUILD_LASTEXITCODE=1
        return
    fi

    MODULEPATH=$(pwsh -c 'echo $env:PSModulePath')
    if [ -z "$MODULEPATH" ]
    then
        log_error "Unable to get powershell module path"
        BUILD_LASTEXITCODE=1
        return
    fi

    # split on : and get the first path
    IFS=':' read -ra MODULEPATHS <<< "$MODULEPATH"
    MODULEPATH=${MODULEPATHS[0]}
    PS1FINDMODULEPATH="$MODULEPATH/${PROJECT_PREFIX}Module"

    mkdir -p "$PS1FINDMODULEPATH"

    log "cp $ps1_version_path/${PROJECT_PREFIX}Module.psm1 $PS1FINDMODULEPATH/"
    cp "$ps1_version_path/${PROJECT_PREFIX}Module.psm1" "$PS1FINDMODULEPATH/"

    # add to bin
    bin_path="$base_path/bin"
    script_path="$ps1_version_path/$ps1_version_name.ps1"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?
}

build_python_version () {
    local base_path="$1"
    local py_version_name="$2"

    log "language: python"
    log "version: $py_version_name"

    py_version_path="$base_path/python/$py_version_name"
    log "py_version_path: $py_version_path"

    if [ ! -d "$py_version_path" ]
    then
        log_error "Path not found: $py_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    echo "cd $py_version_path"
    cd "$py_version_path"

    # Set to Yes to use venv
    USE_VENV=$VENV

    # If VENV is not yes, set to yes if a venv directory exists
    if [ "$USE_VENV" != 'yes' -a -d "$py_version_path/venv" ]
    then
        USE_VENV='yes'
    fi

    # PYTHON_VERSIONS=(python3.12 python3.11 python3.10 python3.9)
    # We don't want to use python3.12 yet
    PYTHON_VERSIONS=(python3.11 python3.10 python3.9)
    PYTHON=

    ACTIVE_VENV=

    if [ "$USE_VENV" == 'yes' ]
    then
        log 'Using venv'

        # 3 possibilities:
        # 1. venv exists and is active
        # 2. venv exists and is not active
        # 3. venv does not exist

        if [ -n "$VIRTUAL_ENV" ]
        then
            # 1. venv exists and is active
            log "Already active venv: $VIRTUAL_ENV"
            ACTIVE_VENV="$VIRTUAL_ENV"

            PYTHON=$(which python3)
            PYTHON=$(basename "$PYTHON")

        elif [ -d "$py_version_path/venv" ]
        then
            # 2. venv exists and is not active
            log 'Using existing venv'

            # activate the venv - we run this even if this venv or another is already active
            # because it's the only way to be able to run deactivate later
            log "source $py_version_path/venv/bin/activate"
            source $py_version_path/venv/bin/activate

            PYTHON=$(which python3)
            PYTHON=$(basename "$PYTHON")

        else
            # 3. venv does not exist
            # ensure python3.9+ is installed
            for p in ${PYTHON_VERSIONS[*]}
            do
                PYTHON=$(which "$p")
                if [ -f "$PYTHON" ]
                then
                    break
                fi
            done

            if [ -z "$PYTHON" ]
            then
                log_error "A version of python >= 3.9 is required"
                BUILD_LASTEXITCODE=1
                return
            else
                PYTHON=$(basename "$PYTHON")
            fi

            log "Creating new venv"

            # create a virtual env to run from and install to if it doesn't already exist
            log "$PYTHON -m venv venv"
            "$PYTHON" -m venv venv

            # activate the venv
            log "source $py_version_path/venv/bin/activate"
            source $py_version_path/venv/bin/activate

            # get the path to the venv version
            PYTHON=$(which python3)
            PYTHON=$(basename "$PYTHON")
        fi

    else

        log "Not using venv"

        # ensure python3.9+ is installed
        for p in ${PYTHON_VERSIONS[*]}
        do
            PYTHON=$(which "$p")
            if [ -f "$PYTHON" ]
            then
                break
            fi
        done

        if [ -z "$PYTHON" ]
        then
            log_error "A version of python >= 3.9 is required"
            BUILD_LASTEXITCODE=1
            return
        else
            PYTHON=$(basename "$PYTHON")
        fi
    fi

    log "Using $PYTHON ($(which $PYTHON))"
    log "python version: $($PYTHON -V)"

    # # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    RESOURCES_PATH="$py_version_path/$py_version_name/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # install wheel - this seems to fix problems with installing local dependencies,
    # which pyfind will be for pysearch
    # log "pip3 install wheel"
    # pip3 install wheel

    # install dependencies in requirements.txt
    log "pip3 install -r requirements.txt"
    pip3 install -r requirements.txt

    # check for success/failure
    ERROR=
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        ERROR=yes
    fi

    # if there was not an active venv before the build, deactivate the venv
    if [ "$USE_VENV" == 'yes' -a -z "$ACTIVE_VENV" ]
    then
        # deactivate at end of setup process
        log "deactivate"
        deactivate
    fi

    if [ -n "$ERROR" ]
    then
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # TODO: change the !# line in pyfind to use the determined python version

    # add to bin
    bin_path="$base_path/bin"
    script_path="$py_version_path/bin/$py_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_ruby_version () {
    local base_path="$1"
    local rb_version_name="$2"

    log "language: ruby"
    log "version: $rb_version_name"

    # ensure ruby3.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        log_error "You need to install ruby"
        BUILD_LASTEXITCODE=1
        return
    fi

    RUBY_VERSION="$(ruby -v 2>&1 | grep '^ruby 3')"
    if [ -z "$RUBY_VERSION" ]
    then
        log_error "A version of ruby >= 3.x is required"
        BUILD_LASTEXITCODE=1
        return
    fi
    log "ruby version: $RUBY_VERSION"

    if [ -z "$(which bundle)" ]
    then
        log_error "You need to install bundler: https://bundler.io/"
        BUILD_LASTEXITCODE=1
        return
    fi

    BUNDLE_VERSION="$(bundle version)"
    log "bundle version: $BUNDLE_VERSION"

    rb_version_path="$base_path/ruby/$rb_version_name"
    log "rb_version_path: $rb_version_path"

    if [ ! -d "$rb_version_path" ]
    then
        log_error "Path not found: $rb_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$rb_version_path/data"
    TEST_RESOURCES_PATH="$rb_version_path/test/fixtures"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    TEST_FILE_PATH=$SHARED_PATH/testFiles
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_FILE_PATH" "$TEST_RESOURCES_PATH"

    # TODO: figure out how to install dependencies without installing rbfind (which is what bundler does)
    echo "cd $rb_version_path"
    cd "$rb_version_path"

    log "bundle install"
    bundle install

    # check for success/failure
    if [ "$?" -ne 0 ]
    then
        log_error "bundle install failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # Build the gem
    log "gem build $rb_version_name.gemspec"
    gem build $rb_version_name.gemspec

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # TODO: install the gem?
    log "gem install $rb_version_name-0.1.0.gem"
    gem install $rb_version_name-0.1.0.gem

    # add to bin
    bin_path="$base_path/bin"
    script_path="$rb_version_path/bin/$rb_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_rust_version () {
    local base_path="$1"
    local rs_version_name="$2"

    log "language: rust"
    log "version: $rs_version_name"

    # ensure rust is installed
    if [ -z "$(which rustc)" ]
    then
        log_error "You need to install rust"
        BUILD_LASTEXITCODE=1
        return
    fi

    RUST_VERSION=$(rustc --version)
    log "rustc version: $RUST_VERSION"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo"
        BUILD_LASTEXITCODE=1
        return
    fi

    CARGO_VERSION=$(cargo --version)
    log "cargo version: $CARGO_VERSION"

    rs_version_path="$base_path/rust/$rs_version_name"
    log "rs_version_path: $rs_version_path"

    if [ ! -d "$rs_version_path" ]
    then
        log_error "Path not found: $rs_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $rs_version_path"
    cd "$rs_version_path"

    if [ -n "$DEBUG" ]
    then
        log "cargo build"
        cargo build

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            cd -
            BUILD_LASTEXITCODE=1
            return
        fi
    fi

    bin_path="$base_path/bin"
    script_path=""

    if [ -n "$RELEASE" ]
    then
        log "cargo build --release"
        cargo build --release

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            cd -
            BUILD_LASTEXITCODE=1
            return
        fi

        # add release to bin
        script_path="$rs_version_path/bin/$rs_version_name.release.sh"
    else
        # add debug to bin
        script_path="$rs_version_path/bin/$rs_version_name.debug.sh"
    fi

    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_scala_version () {
    local base_path="$1"
    local scala_version_name="$2"

    log "language: scala"
    log "version: $scala_version_name"

    # ensure scala is installed
    if [ -z "$(which scala)" ]
    then
        log_error "You need to install scala"
        BUILD_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    # SBT_OUTPUT=$(sbt --version)

    # SBT_PROJECT_VERSION=$(echo "$SBT_OUTPUT" | grep 'project')
    # log "$SBT_PROJECT_VERSION"

    # SBT_SCRIPT_VERSION=$(echo "$SBT_OUTPUT" | grep 'script')
    # log "$SBT_SCRIPT_VERSION"

    JDK_VERSION=$(java -version  2>&1 | head -n 1)
    log "JDK version: $JDK_VERSION"

    scala_version_path="$base_path/scala/$scala_version_name"
    log "scala_version_path: $scala_version_path"

    if [ ! -d "$scala_version_path" ]
    then
        log_error "Path not found: $scala_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    RESOURCES_PATH="$scala_version_path/src/main/resources"
    TEST_RESOURCES_PATH="$scala_version_path/src/test/resources"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    TEST_FILE_PATH=$SHARED_PATH/testFiles
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_FILE_PATH" "$TEST_RESOURCES_PATH"

    log "cd $scala_version_path"
    cd "$scala_version_path"

    # run sbt assembly
    # log "sbt clean assembly"
    # sbt clean assembly
    # to build without testing, changed to this:
    log "sbt 'set test in assembly := {}' clean package assembly"
    sbt 'set test in assembly := {}' clean package assembly

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # copy the jar to xsearch if found
    # TODO: move this to the xfind_build script?
    # if [ -n "$XSEARCH_PATH" -a -d "$XSEARCH_PATH/scala/scalasearch/lib" ]
    # then
    #     SCALAFIND_JAR=$(find "$SCALAFIND_PATH/target/scala-$SCALA_VERSION/" -name "scalafind_3-"*.jar | head -n 1)
    #     if [ -n "$SCALAFIND_JAR" -a -f "$SCALAFIND_JAR" ]
    #     then
    #         log "Copying $SCALAFIND_JAR to $XSEARCH_PATH/scala/scalasearch/lib/"
    #         cp "$SCALAFIND_JAR" "$XSEARCH_PATH/scala/scalasearch/lib/"
    #     else
    #         log_error "Unable to find built scalafind jar to copy to xsearch"
    #     fi
    # fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$scala_version_path/bin/$scala_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_swift_version () {
    local base_path="$1"
    local swift_version_name="$2"

    log "language: swift"
    log "version: $swift_version_name"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        BUILD_LASTEXITCODE=1
        return
    fi

    # swift --version 2>&1 output looks like this:
    # (stdout) Apple Swift version 6.0.2 (swiftlang-6.0.2.1.2 clang-1600.0.26.4)
    # (stdout) Target: x86_64-apple-macosx14.0
    # (stderr) swift-driver version: 1.115
    SWIFT_VERSION=$(swift --version 2>&1 | grep 'Apple Swift' | cut -d ' ' -f 7)
    log "swift version: Apple Swift version $SWIFT_VERSION"

    # TODO: copy resource files locally? - embedded resources not currently supported apparently

    swift_version_path="$base_path/swift/$swift_version_name"
    log "swift_version_path: $swift_version_path"

    if [ ! -d "$swift_version_path" ]
    then
        log_error "Path not found: $swift_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $swift_version_path"
    cd "$swift_version_path"

    # run swift build
    if [ -n "$DEBUG" ]
    then
        log "swift build"
        swift build

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            cd -
            BUILD_LASTEXITCODE=1
            return
        fi
    fi

    bin_path="$base_path/bin"
    script_path=""

    if [ -n "$RELEASE" ]
    then
        log "swift build --configuration release"
        swift build --configuration release

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            cd -
            BUILD_LASTEXITCODE=1
            return
        fi

        # add release to bin
        script_path="$swift_version_path/bin/$swift_version_name.release.sh"
    else
        # add debug to bin
        script_path="$swift_version_path/bin/$swift_version_name.debug.sh"
    fi

    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}

build_typescript_version () {
    local base_path="$1"
    local ts_version_name="$2"

    log "language: typescript"
    log "version: $ts_version_name"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node.js"
        BUILD_LASTEXITCODE=1
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        BUILD_LASTEXITCODE=1
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    ts_version_path="$base_path/typescript/$ts_version_name"
    log "ts_version_path: $ts_version_path"

    # copy the shared json files to the local resource location
    SHARED_PATH="$base_path/shared"
    RESOURCES_PATH="$ts_version_path/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$SHARED_PATH" "$RESOURCES_PATH"

    log "cd $ts_version_path"
    cd "$ts_version_path"

    # run npm install and build
    log "npm install"
    npm install
    log "npm run build"
    npm run build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        cd -
        BUILD_LASTEXITCODE=1
        return
    fi

    # add to bin
    bin_path="$base_path/bin"
    script_path="$ts_version_path/bin/$ts_version_name.sh"
    log "add_to_bin $bin_path $script_path"
    add_to_bin "$bin_path" "$script_path"

    BUILD_LASTEXITCODE=$?

    cd -
}
