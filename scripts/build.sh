#!/bin/bash
################################################################################
#
# build.sh
#
# Builds specified language version of xfind, or all versions
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
    echo -e "\nUsage: build.sh [-h|--help] [--debug] [--release] [--venv] {\"all\" | lang [lang...]}\n"
    exit
}

# copy_config_resources
copy_config_resources () {
    local resources_path="$1"
    log "cp $XFIND_SHARED_PATH/config.json $resources_path/"
    cp "$XFIND_SHARED_PATH/config.json" "$resources_path/"
}

# copy_filetypes_resources
copy_filetypes_resources () {
    local resources_path="$1"
    log "cp $XFIND_SHARED_PATH/filetypes.json $resources_path/"
    cp "$XFIND_SHARED_PATH/filetypes.json" "$resources_path/"
}

# copy_findoptions_resources
copy_findoptions_resources () {
    local resources_path="$1"
    log "cp $XFIND_SHARED_PATH/findoptions.json $resources_path/"
    cp "$XFIND_SHARED_PATH/findoptions.json" "$resources_path/"
}

# copy_json_resources
copy_json_resources () {
    local resources_path="$1"
    copy_config_resources "$resources_path"
    copy_filetypes_resources "$resources_path"
    copy_findoptions_resources "$resources_path"
}

# copy_xml_resources
copy_xml_resources () {
    local resources_path="$1"
    log "cp $XFIND_SHARED_PATH/filetypes.xml $resources_path/"
    cp "$XFIND_SHARED_PATH"/filetypes.xml "$resources_path/"
    log "cp $XFIND_SHARED_PATH/findoptions.xml $resources_path/"
    cp "$XFIND_SHARED_PATH/findoptions.xml" "$resources_path/"
}

# copy_test_resources
copy_test_resources () {
    local test_resources_path="$1"
    log "cp $XFIND_TEST_FILE_PATH/testFile*.txt $test_resources_path/"
    cp "$XFIND_TEST_FILE_PATH"/testFile*.txt "$test_resources_path/"
}

# add_to_bin
add_to_bin () {
    local script_path="$1"
    local script_name=$(basename "$1")
    if [ ! -d "$XFIND_BIN_PATH" ]
    then
        log "Creating bin path"
        log "mkdir -p $XFIND_BIN_PATH"
        mkdir -p "$XFIND_BIN_PATH"
    fi

    cd "$XFIND_BIN_PATH"

    if [[ $script_name == *.sh ]]
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

########################################
# Build Functions
########################################

build_c () {
    echo
    hdr "build_c"

    # ensure make is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        return
    fi

    cd "$CFIND_PATH"

    if [ -n "$DEBUG" ] && [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(debug release)
    elif [ -n "$DEBUG" ]
    then
        CONFIGURATIONS=(debug)
    elif [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(release)
    fi

    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR="cmake-build-$c"
        CMAKE_BUILD_PATH="$CFIND_PATH/$CMAKE_BUILD_DIR"
        CMAKE_BUILD_TYPE="$c"

        if [ ! -d "$CMAKE_BUILD_PATH" ]
        then
            log "mkdir -p $CMAKE_BUILD_PATH"
            mkdir -p "$CMAKE_BUILD_PATH"

            log "cd $CMAKE_BUILD_PATH"
            cd "$CMAKE_BUILD_PATH"

            log "cmake -G \"Unix Makefiles\" -DCMAKE_BUILD_TYPE=$c .."
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # exec 5>&1
            # log "make -f Makefile"
            # OUTPUT=$(make -f Makefile | tee >(cat - >&5))
            # I=$(echo "$OUTPUT" | grep "\[100%\] Built target ")
            # make -f Makefile

            cd -
        fi

        if [ -d "$CMAKE_BUILD_PATH" ]
        then
            TARGETS=(clean cfind cfindapp cfind-tests)
            for t in ${TARGETS[*]}
            do
                log "cmake --build $CMAKE_BUILD_DIR --config $c --target $t"
                cmake --build "$CMAKE_BUILD_DIR" --config "$c" --target "$t"

                # check for success/failure
                # [ "$?" -ne 0 ] && log "An error occurred while trying to run build target $t" >&2 && exit 1
                if [ "$?" -eq 0 ]
                then
                    log "Build target $t succeeded"
                else
                    log_error "Build target $t failed"
                    return
                fi
            done

            # now do the install
            INSTALL_FILES=Y
            if [ -n "$INSTALL_FILES" ]
            then
                log "Installing cfind files"
                log "cmake --install $CMAKE_BUILD_DIR --config $c --prefix /usr/local"
                cmake --install "$CMAKE_BUILD_DIR" --config "$c" --prefix /usr/local
            fi
        fi
    done

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        add_to_bin "$CFIND_PATH/bin/cfind.release.sh"
    else
        # add debug to bin
        add_to_bin "$CFIND_PATH/bin/cfind.debug.sh"
    fi

    cd -
}

build_clojure () {
    echo
    hdr "build_clojure"

    # ensure leiningen is installed
    if [ -z "$(which lein)" ]
    then
        log_error "You need to install leiningen"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$CLJFIND_PATH/resources"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    cd "$CLJFIND_PATH"

    # Create uberjar with lein
    log "Building cljfind"
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
        return
    fi

    # add to bin
    add_to_bin "$CLJFIND_PATH/bin/cljfind.sh"

    cd -
}

build_cpp () {
    echo
    hdr "build_cpp"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        return
    fi

    cd "$CPPFIND_PATH"

    # CMAKE_CXX_FLAGS="-W -Wall -Werror"
    CMAKE_CXX_FLAGS="-W -Wall -Werror -Wextra -Wshadow -Wnon-virtual-dtor -pedantic"

    # Add AddressSanitizer
    # CMAKE_CXX_FLAGS="$CMAKE_CXX_FLAGS -fsanitize=address -fno-omit-frame-pointer"

    if [ -n "$DEBUG" ] && [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(debug release)
    elif [ -n "$DEBUG" ]
    then
        CONFIGURATIONS=(debug)
    elif [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(release)
    fi

    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR="cmake-build-$c"
        CMAKE_BUILD_PATH="$CPPFIND_PATH/$CMAKE_BUILD_DIR"
        CMAKE_BUILD_TYPE="$c"

        if [ ! -d "$CMAKE_BUILD_PATH" ]
        then
            log "mkdir -p $CMAKE_BUILD_PATH"
            mkdir -p "$CMAKE_BUILD_PATH"

            log "cd $CMAKE_BUILD_PATH"
            cd "$CMAKE_BUILD_PATH"

            log "cmake -G \"Unix Makefiles\" -DCMAKE_BUILD_TYPE=$c .."
            cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$c ..

            # exec 5>&1
            # log "make -f Makefile"
            # OUTPUT=$(make -f Makefile | tee >(cat - >&5))
            # I=$(echo "$OUTPUT" | grep "\[100%\] Built target ")
            # make -f Makefile

            cd -
        fi

        if [ -d "$CMAKE_BUILD_PATH" ]
        then
            TARGETS=(clean cppfind cppfindapp cppfind-tests)
            for t in ${TARGETS[*]}
            do
                log "cmake --build $CMAKE_BUILD_DIR --config $c --target $t -- $CMAKE_CXX_FLAGS"
                cmake --build "$CMAKE_BUILD_DIR" --config "$c" --target "$t" -- "$CMAKE_CXX_FLAGS"

                # check for success/failure
                # [ "$?" -ne 0 ] && log "An error occurred while trying to run build target $t" >&2 && exit 1
                if [ "$?" -eq 0 ]
                then
                    log "Build target $t succeeded"
                else
                    log_error "Build target $t failed"
                    return
                fi
            done

            # now do the install
            INSTALL_FILES=Y
            if [ -n "$INSTALL_FILES" ]
            then
                log "Installing cppfind files"
                log "cmake --install $CMAKE_BUILD_DIR --config $c --prefix /usr/local"
                cmake --install "$CMAKE_BUILD_DIR" --config "$c" --prefix /usr/local
            fi
        fi
    done

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        add_to_bin "$CPPFIND_PATH/bin/cppfind.release.sh"
    else
        # add debug to bin
        add_to_bin "$CPPFIND_PATH/bin/cppfind.debug.sh"
    fi

    cd -
}

build_csharp () {
    echo
    hdr "build_csharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        return
    fi

    RESOURCES_PATH="$CSFIND_PATH/CsFindLib/Resources"
    TEST_RESOURCES_PATH="$CSFIND_PATH/CsFindTests/Resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    if [ -n "$DEBUG" ] && [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Debug Release)
    elif [ -n "$DEBUG" ]
    then
        CONFIGURATIONS=(Debug)
    elif [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Release)
    fi

    # run dotnet build for selected configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building csfind for $c configuration"
        log "dotnet build $CSFIND_PATH/CsFind.sln --configuration $c"
        dotnet build "$CSFIND_PATH/CsFind.sln" --configuration "$c"

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            return
        fi
    done

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        add_to_bin "$CSFIND_PATH/bin/csfind.release.sh"
    else
        # add debug to bin
        add_to_bin "$CSFIND_PATH/bin/csfind.debug.sh"
    fi
}

build_dart () {
    echo
    hdr "build_dart"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        return
    fi

    cd "$DARTFIND_PATH"

    # RESOURCES_PATH="$DARTFIND_PATH/lib/data"

    # TODO: move resources to local location, for now read relative to XFIND_PATH
    # mkdir -p "$RESOURCES_PATH"
    # copy_json_resources "$RESOURCES_PATH"

    log "Building dartfind"
    if [ ! -f "$DARTFIND_PATH/.dart_tool/package_config.json" ] && [ ! -f "$DARTFIND_PATH/.packages" ]
    then
        log "dart pub get"
        dart pub get
    else
        log "dart pub upgrade"
        dart pub upgrade
    fi

    log "Compiling dartfind"
    log "dart compile exe $DARTFIND_PATH/bin/dartfind.dart"
    dart compile exe "$DARTFIND_PATH/bin/dartfind.dart"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$DARTFIND_PATH/bin/dartfind.sh"

    cd -
}

build_elixir () {
    echo
    hdr "build_elixir"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        return
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        return
    fi

    cd "$EXFIND_PATH"

    log "Building exfind"
    log "mix deps.get"
    mix deps.get

    log "Creating exfind executable"
    log "mix escript.build"
    mix escript.build

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$EXFIND_PATH/bin/exfind"

    cd -
}

build_fsharp () {
    echo
    hdr "build_fsharp"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        return
    fi

    RESOURCES_PATH="$FSFIND_PATH/FsFindLib/Resources"
    TEST_RESOURCES_PATH="$FSFIND_PATH/FsFindTests/Resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    if [ -n "$DEBUG" ] && [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Debug Release)
    elif [ -n "$DEBUG" ]
    then
        CONFIGURATIONS=(Debug)
    elif [ -n "$RELEASE" ]
    then
        CONFIGURATIONS=(Release)
    fi

    # run dotnet build for selected configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building fsfind for $c configuration"
        log "dotnet build $FSFIND_PATH/FsFind.sln --configuration $c"
        dotnet build "$FSFIND_PATH/FsFind.sln" --configuration "$c"

        # check for success/failure
        if [ "$?" -eq 0 ]
        then
            log "Build succeeded"
        else
            log_error "Build failed"
            return
        fi
    done

    if [ -n "$RELEASE" ]
    then
        # add release to bin
        add_to_bin "$FSFIND_PATH/bin/fsfind.release.sh"
    else
        # add debug to bin
        add_to_bin "$FSFIND_PATH/bin/fsfind.debug.sh"
    fi
}

build_go () {
    echo
    hdr "build_go"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        return
    fi

    # build the code to generate the dynamic code for gofind
    #log "Building gengofindcode"
    #echo "go install elocale.com/clarkcb/gofindcodegen/gengofindcode"
    #go install elocale.com/clarkcb/gofindcodegen/gengofindcode

    # run it to generate the dynamic gofind code
    #log "Running gengofindcode"
    #log "gengofindcode"
    #gengofindcode

    cd "$GOFIND_PATH"

    # go fmt the gofind source (for auto-generated code)
    log "Auto-formatting gofind"
    log "go fmt ./..."
    go fmt ./...

    # create the bin dir if it doesn't already exist
    if [ ! -d "$XFIND_BIN_PATH" ]
    then
        mkdir -p "$XFIND_BIN_PATH"
    fi

    # if GOBIN not defined, set to XFIND_BIN_PATH
    # if [ ! -d "$GOBIN" ]
    # then
    #     export GOBIN="$XFIND_BIN_PATH"
    # fi

    # now build/install gofind
    log "Building gofind"
    log "go install ./..."
    GOBIN="$XFIND_BIN_PATH" go install ./...

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    cd -
}

build_groovy () {
    echo
    hdr "build_groovy"

    cd "$GROOVYFIND_PATH"

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

    RESOURCES_PATH="$GROOVYFIND_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$GROOVYFIND_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # run a maven clean build
    log "Building groovyfind"

    # log "gradle --warning-mode all clean jar publishToMavenLocal"
    # gradle --warning-mode all clean jar publishToMavenLocal
    # GRADLE_ARGS="--info --warning-mode all"
    GRADLE_ARGS="--warning-mode all"
    GRADLE_TASKS="clean jar"
    log "$GRADLE $GRADLE_ARGS $GRADLE_TASKS"
    "$GRADLE" $GRADLE_ARGS $GRADLE_TASKS

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$GROOVYFIND_PATH/bin/groovyfind.sh"

    cd -
}

build_haskell () {
    echo
    hdr "build_haskell"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        return
    fi

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

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$HSFIND_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    cd "$HSFIND_PATH/"

    # build with stack (via make)
    log "Building hsfind"
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
        return
    fi

    log "stack install --local-bin-path $XFIND_BIN_PATH"
    stack install --local-bin-path "$XFIND_BIN_PATH"

    cd -
}

build_java () {
    echo
    hdr "build_java"

    # ensure mvn is installed
    if [ -z "$(which mvn)" ]
    then
        log_error "You need to install maven"
        return
    fi

    RESOURCES_PATH="$JAVAFIND_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$JAVAFIND_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # run a maven clean build
    log "Building javafind"
    log "mvn -f $JAVAFIND_PATH/pom.xml clean package install -Dmaven.test.skip=true -Dmaven.plugin.validation=DEFAULT"
    mvn -f "$JAVAFIND_PATH/pom.xml" clean package install -Dmaven.test.skip=true -Dmaven.plugin.validation=DEFAULT

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # # install to local repo so it can be added as a dependency to javasearch
    # log "mvn -f $JAVAFIND_PATH/pom.xml install"
    # mvn -f "$JAVAFIND_PATH/pom.xml" install

    # add to bin
    add_to_bin "$JAVAFIND_PATH/bin/javafind.sh"
}

build_javascript () {
    echo
    hdr "build_javascript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install node.js/npm"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$JSFIND_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    cd "$JSFIND_PATH"

    # run npm install and build
    log "Building jsfind"
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
        return
    fi

    # add to bin
    add_to_bin "$JSFIND_PATH/bin/jsfind.sh"

    cd -
}

build_kotlin () {
    echo
    hdr "build_kotlin"

    # ensure gradle is installed
    if [ -z "$(which gradle)" ]
    then
        log_error "You need to install gradle"
        return
    fi

    RESOURCES_PATH="$KTFIND_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$KTFIND_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # run a maven clean build
    log "Building ktfind"

    cd "$KTFIND_PATH"

    log "gradle --warning-mode all clean jar publishToMavenLocal"
    gradle --warning-mode all clean jar publishToMavenLocal

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$KTFIND_PATH/bin/ktfind.sh"

    cd -
}

build_objc () {
    echo
    hdr "build_objc"

    TARGET=alltargets

    # TODO: copy resource files locally?
    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        return
    fi

    # TODO: copy resource files locally? - embedded resources not currently supported apparently

    cd "$OBJCFIND_PATH"

    # run swift build
    log "Building objcfind"

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
            return
        fi
    fi
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
            return
        fi

        # add release to bin
        add_to_bin "$OBJCFIND_PATH/bin/objcfind.release.sh"
    else
        # add debug to bin
        add_to_bin "$OBJCFIND_PATH/bin/objcfind.debug.sh"
    fi

    cd -
}

build_ocaml () {
    echo
    hdr "build_ocaml"

    cd "$MLFIND_PATH"
    ./build.sh
    # if [ -L ~/bin/mlfind ]
    # then
    #     rm ~/bin/mlfind
    # fi
    ln -sf "$MLFIND_PATH/_build/src/mlfind.native" ~/bin/mlfind
    cd -
}

build_perl () {
    echo
    hdr "build_perl"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        log_error "You need to install perl"
        return
    fi

    if [ -z "$(perl -v | grep 'This is perl 5')" ]
    then
        log_error "A 5.x version of perl is required"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$PLFIND_PATH/share"
    mkdir -p "$RESOURCES_PATH"
    log "cp $XFIND_SHARED_PATH/config.json $RESOURCES_PATH/"
    cp "$XFIND_SHARED_PATH/config.json" "$RESOURCES_PATH/"
    log "cp $XFIND_SHARED_PATH/filetypes.json $RESOURCES_PATH/"
    cp "$XFIND_SHARED_PATH/filetypes.json" "$RESOURCES_PATH/"
    log "cp $XFIND_SHARED_PATH/findoptions.json $RESOURCES_PATH/"
    cp "$XFIND_SHARED_PATH/findoptions.json" "$RESOURCES_PATH/"

    # check for success/failure
    if [ "$?" -eq 0 ]
    then
        log "Build succeeded"
    else
        log_error "Build failed"
        return
    fi

    # add to bin
    add_to_bin "$PLFIND_PATH/bin/plfind.sh"
}

build_php () {
    echo
    hdr "build_php"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        return
    fi

    # TODO: do a real version check
    if [ -z "$(php -v | grep 'cli')" ]
    then
        log_error "A version of PHP >= 7.x is required"
        return
    fi

    # ensure composer is installed
    if [ -z "$(which composer)" ]
    then
        log_error "Need to install composer"
        return
    fi

    CONFIG_PATH="$PHPFIND_PATH/config"
    RESOURCES_PATH="$PHPFIND_PATH/resources"

    # copy the shared config json file to the local config location
    mkdir -p "$CONFIG_PATH"
    log "cp $XFIND_SHARED_PATH/config.json $CONFIG_PATH/"
    cp "$XFIND_SHARED_PATH/config.json" "$CONFIG_PATH/"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    log "cp $XFIND_SHARED_PATH/filetypes.json $RESOURCES_PATH/"
    cp "$XFIND_SHARED_PATH/filetypes.json" "$RESOURCES_PATH/"
    log "cp $XFIND_SHARED_PATH/findoptions.json $RESOURCES_PATH/"
    cp "$XFIND_SHARED_PATH/findoptions.json" "$RESOURCES_PATH/"

    cd "$PHPFIND_PATH"

    # run a composer build
    log "Building phpfind"

    if [ -d "$PHPFIND_PATH/vendor" ]
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
        return
    fi

    # add to bin
    add_to_bin "$PHPFIND_PATH/bin/phpfind.sh"

    cd -
}

build_powershell () {
    echo
    hdr "build_powershell"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        return
    fi

    MODULEPATH=$(pwsh -c 'echo $env:PSModulePath')

    if [ -z "$MODULEPATH" ]
    then
        log_error "Unable to get powershell module path"
        return
    fi

    log "Building ps1find"

    # split on : and get the first path
    IFS=':' read -ra MODULEPATHS <<< "$MODULEPATH"
    MODULEPATH=${MODULEPATHS[0]}
    PS1FINDMODULEPATH="$MODULEPATH/Ps1FindModule"

    log "cp $PS1FIND_PATH/Ps1FindModule.psm1 $PS1FINDMODULEPATH/"
    cp "$PS1FIND_PATH/Ps1FindModule.psm1" "$PS1FINDMODULEPATH/"

    # add to bin
    add_to_bin "$PS1FIND_PATH/ps1find.ps1"
}

build_python () {
    echo
    hdr "build_python"

    # Set to Yes to use venv
    USE_VENV=$VENV
    # PYTHON_VERSIONS=(python3.12 python3.11 python3.10 python3.9)
    # We don't want to use python3.12 yet
    PYTHON_VERSIONS=(python3.11 python3.10 python3.9)
    PYTHON=

    if [ "$USE_VENV" == 'yes' ]
    then
        log 'Using venv'

        if [ -d "$PYFIND_PATH/venv" ]
        then
            log 'Using existing venv'

            # if venv is active, deactivate it (in case it happens to be another venv that is active)
            if [ -n "$VIRTUAL_ENV" ]
            then
                log 'Deactivating current venv'
                deactivate
            fi

            # if venv isn't active, activate it
            # (TODO: this is probably always true because of earlier deactivation)
            if [ -z "$VIRTUAL_ENV" ]
            then
                log "source $PYFIND_PATH/venv/bin/activate"
                source $PYFIND_PATH/venv/bin/activate
            fi

            # PYTHON=$(find "$PYFIND_PATH/venv/bin" -name python3 | head -n 1)
            PYTHON=$(which python3)
            PYTHON=$(basename "$PYTHON")
        else
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
                return
            else
                PYTHON=$(basename "$PYTHON")
            fi

            log "Creating new venv"

            # create a virtual env to run from and install to if it doesn't already exist
            log "$PYTHON -m venv venv"
            "$PYTHON" -m venv venv

            # activate the venv
            log "source $PYFIND_PATH/venv/bin/activate"
            source $PYFIND_PATH/venv/bin/activate

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
            return
        else
            PYTHON=$(basename "$PYTHON")
        fi
    fi

    log "Using $PYTHON ($(which $PYTHON))"
    log "$PYTHON -V: $($PYTHON -V)"

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$PYFIND_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"
    # TODO: this next path is the *real* resource path, need to remove the other one
    RESOURCES_PATH="$PYFIND_PATH/pyfind/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    cd "$PYFIND_PATH"

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

    if [ "$USE_VENV" == 'yes' ]
    then
        # deactivate at end of setup process
        log "deactivate"
        deactivate
    fi

    if [ -n "$ERROR" ]
    then
        return
    fi

    # TODO: change the !# line in pyfind to use the determined python version

    # add to bin
    add_to_bin "$PYFIND_PATH/bin/pyfind.sh"

    cd -
}

build_ruby () {
    echo
    hdr "build_ruby"

    # ensure ruby2.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        log_error "You need to install ruby"
        return
    fi

    # TODO: do a real version check (first determine minimum needed version)
    if [ -z "$(ruby -v | grep 'ruby 3')" ]
    then
        log_error "A version of ruby >= 3.x is required"
        return
    fi

    # if [ -z "$(which bundle)" ]
    # then
    #     log_error "You need to install bundler: https://bundler.io/"
    #     return
    # fi

    RESOURCES_PATH="$RBFIND_PATH/data"
    TEST_RESOURCES_PATH="$RBFIND_PATH/lib/test/fixtures"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the shared test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    # TODO: figure out how to install dependencies without installing rbfind (which is what bundler does)
    # cd "$RBFIND_PATH"
    # log "bundle"
    # bundle
    # cd -

    # add to bin
    add_to_bin "$RBFIND_PATH/bin/rbfind.sh"
}

build_rust () {
    echo
    hdr "build_rust"

    # ensure cargo/rust is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install rust"
        return
    fi

    cd "$RSFIND_PATH"

    log "Building rsfind"
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
            return
        fi
    fi
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
            return
        fi

        # add release to bin
        add_to_bin "$RSFIND_PATH/bin/rsfind.release.sh"
    else
        # add debug to bin
        add_to_bin "$RSFIND_PATH/bin/rsfind.debug.sh"
    fi

    cd -
}

build_scala () {
    echo
    hdr "build_scala"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        log_error "You need to install scala + sbt"
        return
    fi

    RESOURCES_PATH="$SCALAFIND_PATH/src/main/resources"
    TEST_RESOURCES_PATH="$SCALAFIND_PATH/src/test/resources"

    # copy the shared json files to the local resource location
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    # copy the test files to the local test resource location
    mkdir -p "$TEST_RESOURCES_PATH"
    copy_test_resources "$TEST_RESOURCES_PATH"

    cd "$SCALAFIND_PATH"

    # run sbt assembly
    log "Building scalafind"
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
        return
    fi

    # add to bin
    add_to_bin "$SCALAFIND_PATH/bin/scalafind.sh"

    cd -
}

build_swift () {
    echo
    hdr "build_swift"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        return
    fi

    # TODO: copy resource files locally? - embedded resources not currently supported apparently

    cd "$SWIFTFIND_PATH"

    # run swift build
    log "Building swiftfind"

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
            return
        fi
    fi
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
            return
        fi

        # add release to bin
        add_to_bin "$SWIFTFIND_PATH/bin/swiftfind.release.sh"
    else
        # add debug to bin
        add_to_bin "$SWIFTFIND_PATH/bin/swiftfind.debug.sh"
    fi

    cd -
}

build_typescript () {
    echo
    hdr "build_typescript"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install node.js/npm"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH="$TSFIND_PATH/data"
    mkdir -p "$RESOURCES_PATH"
    copy_json_resources "$RESOURCES_PATH"

    cd "$TSFIND_PATH"

    # run npm install and build
    log "Building tsfind"
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
        return
    fi

    # add to bin
    add_to_bin "$TSFIND_PATH/bin/tsfind.sh"

    cd -
}

# build_linux - builds the versions that are currently supported in the linux container
# Notes about some of the builds:
# - build_clojure    - this build is _really_ slow (10+ minutes?), so call its build directly if you want to try it
# - build_cpp        - this build takes a decent amount of time to complete (though nowhere near as much as clojure)
# - build_go         - go is known for having very fast builds, and it's true, the only builds that are faster here
#                      are the ones that do nothing except copy over resources files (e.g. perl)
# - build_haskell    - having some dependency issues that need to work through to get it buildling again
# - build_javascript - this fails to build in the vscode terminal right now due to some debug plugin issue; building
#                      in an external terminal fixes the problem
# - build_kotlin     - This build can sometimes be quite slow, other times fairly fast. In particular, the first
#                      time will likely be quite slow, and I think it will also be slow when a build hasn't been run
#                      in a while
# - build_objc       - not sure if it's even possible to build this on linux, but excluding for now
# - build_ocaml      - had a number of different issues trying to get this version building again, finally
#                      gave up for now after it appeared that there were a lot of changes to the main API, etc.
# - build_rust       - the first time this build is run it will pretty time-consuming, particularly for release
#                      target, but intermittent builds should be pretty fast
# - build_scala      - this build isn't as slow as the clojure version's, but it's slow enough to run separately
# - build_typescript - this build has the same problem as build_javascript; run the build in an external terminal
build_linux () {
    hdr "build_linux"

    time build_c

    # time build_clojure

    # time build_cpp

    time build_csharp

    time build_dart

    time build_fsharp

    time build_go

    # time build_groovy

    time build_java

    time build_javascript

    # time build_kotlin

    time build_perl

    time build_php

    # time build_powershell

    time build_python

    time build_ruby

    time build_rust

    # time build_scala

    time build_swift

    time build_typescript
}

build_all () {
    hdr "build_all"

    time build_c

    time build_clojure

    time build_cpp

    time build_csharp

    time build_dart

    time build_elixir

    time build_fsharp

    time build_go

    time build_groovy

    time build_haskell

    time build_java

    time build_javascript

    time build_kotlin

    time build_objc

    # time build_ocaml

    time build_perl

    time build_php

    time build_powershell

    time build_python

    time build_ruby

    time build_rust

    time build_scala

    time build_swift

    time build_typescript
}


########################################
# Build Main
########################################
HELP=
DEBUG=
RELEASE=
VENV=
BUILD_ALL=
# TARGET_LANG=all
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
        --debug)
            DEBUG=yes
            ;;
        --release)
            RELEASE=yes
            ;;
        --venv)
            VENV=yes
            ;;
        --all | all)
            BUILD_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

if [ -z "$DEBUG" ] && [ -z "$RELEASE" ]
then
    DEBUG=yes
fi

# log the settings
log "HELP: $HELP"
log "DEBUG: $DEBUG"
log "RELEASE: $RELEASE"
log "VENV: $VENV"
log "BUILD_ALL: $BUILD_ALL"
log "TARGET_LANGS: ${TARGET_LANGS[*]}"

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$BUILD_ALL" ]
then
    build_all
    exit
fi

if [ ${#TARGET_LANGS[@]} == 0 ]
then
    usage
fi

for TARGET_LANG in ${TARGET_LANGS[*]}
do
    case $TARGET_LANG in
        linux)
            build_linux
            ;;
        c)
            build_c
            ;;
        clj | clojure)
            build_clojure
            ;;
        cpp)
            build_cpp
            ;;
        cs | csharp)
            build_csharp
            ;;
        dart)
            build_dart
            ;;
        ex | elixir)
            build_elixir
            ;;
        fs | fsharp)
            build_fsharp
            ;;
        go)
            build_go
            ;;
        groovy)
            build_groovy
            ;;
        haskell | hs)
            build_haskell
            ;;
        java)
            build_java
            ;;
        javascript | js)
            build_javascript
            ;;
        kotlin | kt)
            build_kotlin
            ;;
        objc)
            build_objc
            ;;
        # ocaml | ml)
        #     build_ocaml
        #     ;;
        perl | pl)
            build_perl
            ;;
        php)
            build_php
            ;;
        ps1 | powershell)
            build_powershell
            ;;
        py | python)
            build_python
            ;;
        rb | ruby)
            build_ruby
            ;;
        rs | rust)
            build_rust
            ;;
        scala)
            build_scala
            ;;
        swift)
            build_swift
            ;;
        ts | typescript)
            build_typescript
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done
