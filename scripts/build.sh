#!/bin/sh
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

# copy_json_resources
copy_json_resources () {
    local resources_path="$1"
    log "cp $SHARED_PATH/config.json $resources_path/"
    cp $SHARED_PATH/config.json $resources_path/
    log "cp $SHARED_PATH/filetypes.json $resources_path/"
    cp $SHARED_PATH/filetypes.json $resources_path/
    log "cp $SHARED_PATH/findoptions.json $resources_path/"
    cp $SHARED_PATH/findoptions.json $resources_path/
}

# copy_xml_resources
copy_xml_resources () {
    local resources_path="$1"
    log "cp $SHARED_PATH/filetypes.xml $resources_path/"
    cp $SHARED_PATH/filetypes.xml $resources_path/
    log "cp $SHARED_PATH/findoptions.xml $resources_path/"
    cp $SHARED_PATH/findoptions.xml $resources_path/
}

# copy_test_resources
copy_test_resources () {
    local test_resources_path="$1"
    log "cp $TEST_FILE_PATH/testFile*.txt $test_resources_path/"
    cp $TEST_FILE_PATH/testFile*.txt $test_resources_path/
}

# add_to_bin
add_to_bin () {
    local script_path="$1"
    local script_name=$(basename $1)
    if [ ! -d "$BIN_PATH" ]
    then
        log "Creating bin path"
        log "mkdir -p $BIN_PATH"
        mkdir -p $BIN_PATH
    fi

    cd $BIN_PATH

    if [[ $script_name == *.sh ]]
    then
        script_name=${script_name%.*}
    fi

    if [ -f "$script_name" ]
    then
        log "rm $script_name"
        rm "$script_name"
    fi

    log "ln -s $script_path $script_name"
    ln -s $script_path $script_name

    cd -
}

########################################
# Build Functions
########################################

build_clojure () {
    echo
    hdr "build_clojure"

    if [ -z "$(which lein)" ]
    then
        echo "You need to install leiningen"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH=$CLJFIND_PATH/resources
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH
    # copy_xml_resources $RESOURCES_PATH

    cd $CLJFIND_PATH

    # Create uberjar with lein
    log "Building cljfind"
    log "lein clean"
    lein clean
    log "lein uberjar"
    lein uberjar

    # add to bin
    add_to_bin "$CLJFIND_PATH/bin/cljfind.sh"

    cd -
}

build_cpp () {
    echo
    hdr "build_cpp"

    if [ -z "$(which cmake)" ]
    then
        echo "You need to install cmake"
        return
    fi

    cd $CPPFIND_PATH

    CONFIGURATIONS=(debug release)
    for c in ${CONFIGURATIONS[*]}
    do
        CMAKE_BUILD_DIR="cmake-build-$c"
        CMAKE_BUILD_PATH=$CPPFIND_PATH/$CMAKE_BUILD_DIR

        if [ ! -d "$CMAKE_BUILD_PATH" ]
        then
            log "mkdir -p $CMAKE_BUILD_PATH"
            mkdir -p $CMAKE_BUILD_PATH

            log "cd $CMAKE_BUILD_PATH"
            cd $CMAKE_BUILD_PATH

            log "cmake -G \"Unix Makefiles\" .."
            cmake -G "Unix Makefiles" ..

            # exec 5>&1
            log "make -f Makefile"
            # OUTPUT=$(make -f Makefile | tee >(cat - >&5))
            # I=$(echo "$OUTPUT" | grep "\[100%\] Built target ")
            make -f Makefile

            cd -
        fi

        if [ -d "$CMAKE_BUILD_PATH" ]
        then
            TARGETS=(clean cppfind cppfind-tests)
            for t in ${TARGETS[*]}
            do
                log "cmake --build $CMAKE_BUILD_DIR --target $t -- -W -Wall -Werror"
                cmake --build $CMAKE_BUILD_DIR --target $t -- -W -Wall -Werror
            done
        fi
    done

    # add to bin
    add_to_bin "$CPPFIND_PATH/bin/cppfind.sh"

    cd -
}

build_csharp () {
    echo
    hdr "build_csharp"

    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    RESOURCES_PATH=$CSFIND_PATH/CsFind/Resources
    TEST_RESOURCES_PATH=$CSFIND_PATH/CsFindTests/Resources
    CONFIGURATIONS=(Debug Release)

    # copy the shared json, xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    # copy the shared test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run dotnet build for both configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building csfind for $c configuration"
        log "dotnet build $CSFIND_PATH/CsFind.sln --configuration $c"
        dotnet build $CSFIND_PATH/CsFind.sln --configuration $c
    done

    # add to bin
    add_to_bin "$CSFIND_PATH/bin/csfind.sh"
}

build_dart () {
    echo
    hdr "build_dart"

    if [ -z "$(which dart)" ]
    then
        echo "You need to install dart"
        return
    fi

    cd $DARTFIND_PATH

    # RESOURCES_PATH=$DARTFIND_PATH/lib/data

    # TODO: move resources to local location, for now read relative to XFIND_PATH
    # mkdir -p $RESOURCES_PATH
    # copy_json_resources $RESOURCES_PATH

    log "Building dartfind"
    if [ ! -f "$DARTFIND_PATH/.packages" ]
    then
        log "dart pub get"
        dart pub get
    else
        log "dart pub upgrade"
        dart pub upgrade
    fi

    # add to bin
    add_to_bin "$DARTFIND_PATH/bin/dartfind.sh"

    cd -
}

build_fsharp () {
    echo
    hdr "build_fsharp"

    if [ -z "$(which dotnet)" ]
    then
        echo "You need to install dotnet"
        return
    fi

    RESOURCES_PATH=$FSFIND_PATH/FsFind/Resources
    TEST_RESOURCES_PATH=$FSFIND_PATH/FsFindTests/Resources
    CONFIGURATIONS=(Debug Release)

    # copy the shared json, xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH
    copy_xml_resources $RESOURCES_PATH

    # copy the shared test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run dotnet for both configurations
    for c in ${CONFIGURATIONS[*]}
    do
        log "Building fsfind for $c configuration"
        log "dotnet build $FSFIND_PATH/FsFind.sln --configuration $c"
        dotnet build $FSFIND_PATH/FsFind.sln --configuration $c
    done

    # add to bin
    add_to_bin "$FSFIND_PATH/bin/fsfind.sh"
}

build_go () {
    echo
    hdr "build_go"

    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        return
    fi

    cd $GOFIND_PATH

    # go fmt the gofind source (for auto-generated code)
    log "Auto-formatting gofind"
    log "go fmt ./..."
    go fmt ./...

    # create the bin dir if it doesn't already exist
    if [ ! -d "$BIN_PATH" ]
    then
        mkdir -p "$BIN_PATH"
    fi

    # if GOBIN not defined, set to BIN_PATH
    if [ ! -d "$GOBIN" ]
    then
        export GOBIN=$BIN_PATH
    fi

    # now build/install gofind
    log "Building gofind"
    log "go install ./..."
    go install ./...

    cd -
}

build_haskell () {
    echo
    hdr "build_haskell"

    if [ -z "$(which stack)" ]
    then
        echo "You need to install stack"
        return
    fi

    # copy the shared xml files to the local resource location
    RESOURCES_PATH=$HSFIND_PATH/data
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    cd $HSFIND_PATH/

    # build with stack (via make)
    log "Building hsfind"
    log "stack setup"
    make setup

    log "stack build"
    make build

    log "stack install --local-bin-path $BIN_PATH"
    stack install --local-bin-path $BIN_PATH

    cd -
}

build_java () {
    echo
    hdr "build_java"

    if [ -z "$(which mvn)" ]
    then
        echo "You need to install maven"
        return
    fi

    RESOURCES_PATH=$JAVAFIND_PATH/src/main/resources
    TEST_RESOURCES_PATH=$JAVAFIND_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run a maven clean build
    log "Building javafind"
    log "mvn -f $JAVAFIND_PATH/pom.xml clean package -Dmaven.test.skip=true"
    mvn -f $JAVAFIND_PATH/pom.xml clean package -Dmaven.test.skip=true

    # add to bin
    add_to_bin "$JAVAFIND_PATH/bin/javafind.sh"
}

build_javascript () {
    echo
    hdr "build_javascript"

    if [ -z "$(which npm)" ]
    then
        echo "You need to install node.js/npm"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH=$JSFIND_PATH/data
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    cd $JSFIND_PATH

    # run npm install and build
    log "Building jsfind"
    log "npm install"
    npm install

    log "npm run build"
    npm run build

    # add to bin
    add_to_bin "$JSFIND_PATH/bin/jsfind.sh"

    cd -
}

build_kotlin () {
    echo
    hdr "build_kotlin"

    if [ -z "$(which gradle)" ]
    then
        echo "You need to install gradle"
        return
    fi

    RESOURCES_PATH=$KTFIND_PATH/src/main/resources
    TEST_RESOURCES_PATH=$KTFIND_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # run a maven clean build
    log "Building ktfind"

    cd $KTFIND_PATH

    log "gradle -b build.gradle clean jar"
    gradle -b build.gradle clean jar

    # add to bin
    add_to_bin "$KTFIND_PATH/bin/ktfind.sh"

    cd -
}

build_objc () {
    echo
    hdr "build_objc"

    TARGET=alltargets

    if [ -z "$(which xcodebuild)" ]
    then
        echo "You need to install Xcode"
        return
    fi

    # TODO: copy resource files locally?

    cd $OBJCFIND_PATH

    # run xcodebuild
    log "Building objcfind"
    if [ $TARGET == "alltargets" ]
    then
        log "xcodebuild -alltargets"
        xcodebuild -alltargets
    else
        log "xcodebuild -project $TARGET"
        xcodebuild -project $TARGET
    fi

    # add to bin
    add_to_bin "$OBJCFIND_PATH/bin/objcfind.sh"

    cd -
}

build_ocaml () {
    echo
    hdr "build_ocaml"

    cd $MLFIND_PATH
    ./build.sh
    if [ -L ~/bin/mlfind ]
    then
        rm ~/bin/mlfind
    fi
    ln -s $MLFIND_PATH/_build/src/mlfind.native ~/bin/mlfind
    cd -
}

build_perl () {
    echo
    hdr "build_perl"

    if [ -z "$(which perl)" ]
    then
        echo "You need to install perl"
        return
    fi

    if [ -z "$(perl -v | grep 'This is perl 5')" ]
    then
        echo "A 5.x version of perl is required"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH=$PLFIND_PATH/share
    mkdir -p $RESOURCES_PATH
    log "cp $SHARED_PATH/config.json $RESOURCES_PATH/"
    cp $SHARED_PATH/config.json $RESOURCES_PATH/
    log "cp $SHARED_PATH/filetypes.json $RESOURCES_PATH/"
    cp $SHARED_PATH/filetypes.json $RESOURCES_PATH/
    log "cp $SHARED_PATH/findoptions.json $RESOURCES_PATH/"
    cp $SHARED_PATH/findoptions.json $RESOURCES_PATH/

    # add to bin
    add_to_bin "$PLFIND_PATH/bin/plfind.sh"
}

build_php () {
    echo
    hdr "build_php"

    if [ -z "$(which php)" ]
    then
        echo "You need to install PHP"
        return
    fi

    # TODO: do a real version check
    if [ -z "$(php -v | grep 'cli')" ]
    then
        echo "A version of PHP >= 7.x is required"
        return
    fi

    CONFIG_PATH=$PHPFIND_PATH/config
    RESOURCES_PATH=$PHPFIND_PATH/resources

    # copy the shared config json file to the local config location
    mkdir -p $CONFIG_PATH
    log "cp $SHARED_PATH/config.json $CONFIG_PATH/"
    cp $SHARED_PATH/config.json $CONFIG_PATH/

    # copy the shared json files to the local resource location
    mkdir -p $RESOURCES_PATH
    log "cp $SHARED_PATH/filetypes.json $RESOURCES_PATH/"
    cp $SHARED_PATH/filetypes.json $RESOURCES_PATH/
    log "cp $SHARED_PATH/findoptions.json $RESOURCES_PATH/"
    cp $SHARED_PATH/findoptions.json $RESOURCES_PATH/

    COMPOSER=$(which composer)
    if [ -z "$COMPOSER" ]
    then
        echo "Need to install composer"
        return
    fi

    cd $PHPFIND_PATH

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

    # add to bin
    add_to_bin "$PHPFIND_PATH/bin/phpfind.sh"

    cd -
}

build_python () {
    echo
    hdr "build_python"

    if [ -z "$(which python3)" ]
    then
        log "You need to install python (>= 3.7)"
        return
    fi

    PYTHON_VERSIONS=(python3.9 python3.8 python3.7)
    PYTHON=
    for p in ${PYTHON_VERSIONS[*]}
    do
        PYTHON="$(which $p)"
        if [ -f "$PYTHON" ]
        then
            break
        fi
    done

    if [ -z "$PYTHON" ]
    then
        log "A version of python >= 3.7 is required"
        return
    else
        PYTHON=$(basename $PYTHON)
        log "Using $PYTHON"
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH=$PYFIND_PATH/data
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    cd $PYFIND_PATH

    # create a virtual env to run from and install to
    if [ ! -d $PYFIND_PATH/venv ]
    then
        log "$PYTHON -m venv venv"
        $PYTHON -m venv venv
    fi

    # activate the virtual env
    log "source ./venv/bin/activate"
    source ./venv/bin/activate

    # install dependencies in requirements.txt
    log "pip3 install -r requirements.txt"
    pip3 install -r requirements.txt

    # deactivate at end of setup process
    log "deactivate"
    deactivate

    # TODO: change the !# line in pyfind to use the determined python version

    # add to bin
    add_to_bin "$PYFIND_PATH/bin/pyfind.sh"

    cd -
}

build_ruby () {
    echo
    hdr "build_ruby"

    if [ -z "$(which ruby)" ]
    then
        echo "You need to install ruby"
        return
    fi

    # TODO: do a real version check (first determine minimum needed version)
    if [ -z "$(ruby -v | grep 'ruby 2')" ]
    then
        echo "A version of ruby >= 2.x is required"
        return
    fi

    # if [ -z "$(which bundle)" ]
    # then
    #     echo "You need to install bundler: https://bundler.io/"
    #     return
    # fi

    RESOURCES_PATH=$RBFIND_PATH/data
    TEST_RESOURCES_PATH=$RBFIND_PATH/lib/test/fixtures

    # copy the shared json files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    # copy the shared test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    # TODO: figure out how to install dependencies without installing rbfind (which is what bundler does)
    # cd $RBFIND_PATH
    # log "bundle"
    # bundle
    # cd -

    # add to bin
    add_to_bin "$RBFIND_PATH/bin/rbfind.sh"
}

build_rust () {
    echo
    hdr "build_rust"

    if [ -z "$(which cargo)" ]
    then
        echo "You need to install rust"
        return
    fi

    cd $RSFIND_PATH

    log "Building rsfind"
    log "cargo build"
    cargo build
    log "cargo build --release"
    cargo build --release

    # add to bin
    add_to_bin "$RSFIND_PATH/bin/rsfind.sh"

    cd -
}

build_scala () {
    echo
    hdr "build_scala"

    if [ -z "$(which sbt)" ]
    then
        echo "You need to install scala + sbt"
        return
    fi

    RESOURCES_PATH=$SCALAFIND_PATH/src/main/resources
    TEST_RESOURCES_PATH=$SCALAFIND_PATH/src/test/resources

    # copy the shared xml files to the local resource location
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    # copy the test files to the local test resource location
    mkdir -p $TEST_RESOURCES_PATH
    copy_test_resources $TEST_RESOURCES_PATH

    cd $SCALAFIND_PATH

    # run sbt assembly
    log "Building scalafind"
    # log "sbt clean assembly"
    # sbt clean assembly
    # to build without testing, changed to this:
    log "sbt 'set test in assembly := {}' clean assembly"
    sbt 'set test in assembly := {}' clean assembly

    # add to bin
    add_to_bin "$SCALAFIND_PATH/bin/scalafind.sh"

    cd -
}

build_swift () {
    echo
    hdr "build_swift"

    if [ -z "$(which swift)" ]
    then
        echo "You need to install swift"
        return
    fi

    # TODO: copy resource files locally? - embedded resources not currently supported apparently

    cd $SWIFTFIND_PATH

    # run swift build
    log "Building swiftfind"
    # CONFIGURATIONS=(debug release)
    log "swift build"
    swift build

    log "swift build --configuration release"
    swift build --configuration release

    # add to bin
    add_to_bin "$SWIFTFIND_PATH/bin/swiftfind.sh"

    cd -
}

build_typescript () {
    echo
    hdr "build_typescript"

    if [ -z "$(which npm)" ]
    then
        echo "You need to install node.js/npm"
        return
    fi

    # copy the shared json files to the local resource location
    RESOURCES_PATH=$TSFIND_PATH/data
    mkdir -p $RESOURCES_PATH
    copy_json_resources $RESOURCES_PATH

    cd $TSFIND_PATH

    # run npm install and build
    log "Building tsfind"
    log "npm install"
    npm install
    log "npm run build"
    npm run build

    # add to bin
    add_to_bin "$TSFIND_PATH/bin/tsfind.sh"

    cd -
}

build_all () {
    hdr "build_all"

    build_clojure

    build_cpp

    build_csharp

    build_dart

    build_fsharp

    build_go

    build_haskell

    build_java

    build_javascript

    build_kotlin

    build_objc

    build_ocaml

    build_perl

    build_php

    build_python

    build_ruby

    build_rust

    build_scala

    build_swift

    build_typescript
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
    build_all
elif [ "$ARG" == "clojure" ] || [ "$ARG" == "clj" ]
then
    build_clojure
elif [ "$ARG" == "cpp" ]
then
    build_cpp
elif [ "$ARG" == "csharp" ] || [ "$ARG" == "cs" ]
then
    build_csharp
elif [ "$ARG" == "dart" ]
then
    build_dart
elif [ "$ARG" == "fsharp" ] || [ "$ARG" == "fs" ]
then
    build_fsharp
elif [ "$ARG" == "go" ]
then
    build_go
elif [ "$ARG" == "haskell" ] || [ "$ARG" == "hs" ]
then
    build_haskell
elif [ "$ARG" == "java" ]
then
    build_java
elif [ "$ARG" == "javascript" ] || [ "$ARG" == "js" ]
then
    build_javascript
elif [ "$ARG" == "kotlin" ] || [ "$ARG" == "kt" ]
then
    build_kotlin
elif [ "$ARG" == "objc" ]
then
    build_objc
elif [ "$ARG" == "ocaml" ] || [ "$ARG" == "ml" ]
then
    build_ocaml
elif [ "$ARG" == "perl" ] || [ "$ARG" == "pl" ]
then
    build_perl
elif [ "$ARG" == "php" ]
then
    build_php
elif [ "$ARG" == "python" ] || [ "$ARG" == "py" ]
then
    build_python
elif [ "$ARG" == "ruby" ] || [ "$ARG" == "rb" ]
then
    build_ruby
elif [ "$ARG" == "rust" ] || [ "$ARG" == "rs" ]
then
    build_rust
elif [ "$ARG" == "scala" ]
then
    build_scala
elif [ "$ARG" == "swift" ]
then
    build_swift
elif [ "$ARG" == "typescript" ] || [ "$ARG" == "ts" ]
then
    build_typescript
else
    echo "ERROR: unknown build argument: $ARG"
fi
