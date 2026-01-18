#!/bin/bash
################################################################################
#
# clean_functions.sh
#
# Clean functions for xfind/xsearch language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# source "$DIR/config.sh"
source "$DIR/common.sh"

# Global variable to hold last funtion exit code
CLEAN_LASTEXITCODE=0

# Keep track of successful and failed cleans
SUCCESSFUL_CLEANS=()
FAILED_CLEANS=()


########################################
# Utility Functions
########################################

# usage () {
#     echo -e "\nUsage: clean.sh [-h|--help] [--lock] {\"all\" | lang [lang...]}\n"
#     exit
# }

# clean_json_resources
clean_json_resources () {
    local resources_path="$1"
    for f in $(find "$resources_path" -name "*.json" -type f -maxdepth 1)
    do
        log "rm $f"
        rm "$f"
    done
}

# clean_test_resources
clean_test_resources () {
    local resources_path="$1"
    for f in $(find "$resources_path" -name "testFile*.txt" -type f -maxdepth 1)
    do
        log "rm $f"
        rm "$f"
    done
}

print_clean_results () {
    if [ ${#SUCCESSFUL_CLEANS[@]} -gt 0 ]
    then
        log "Successful cleans (${#SUCCESSFUL_CLEANS[@]}): ${SUCCESSFUL_CLEANS[*]}"
    else
        log_error "Successful cleans: 0"
    fi
    if [ ${#FAILED_CLEANS[@]} -gt 0 ]
    then
        log_error "Failed cleans (${#FAILED_CLEANS[@]}): ${FAILED_CLEANS[*]}"
    else
        log "Failed cleans: 0"
    fi
}


########################################
# Clean Functions
########################################

clean_bash_version () {
    echo
    hdr "clean_bash_version"
    log "Nothing to do for bash"
}

clean_c_version () {
    local base_path="$1"
    local c_version_name="$2"

    log "language: C"
    log "version: $c_version_name"

    c_version_path="$base_path/c/$c_version_name"
    log "c_version_path: $c_version_path"

    if [ ! -d "$c_version_path" ]
    then
        log_error "Path not found: $c_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $c_version_path"
    cd "$c_version_path"

    for c in $(find . -name "cmake-build-*" -type d -maxdepth 1)
    do
        log "rm -rf $c"
        rm -rf "$c"

        if [ "$?" -ne 0 ]
        then
            CLEAN_LASTEXITCODE=1
            cd -
            return 1
        fi
    done

    cd -
}

clean_clojure_version () {
    local base_path="$1"
    local clj_version_name="$2"

    log "language: clojure"
    log "version: $clj_version_name"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        log_error "You need to install lein"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    clj_version_path="$base_path/clojure/$clj_version_name"
    log "clj_version_path: $clj_version_path"

    if [ ! -d "$clj_version_path" ]
    then
        log_error "Path not found: $clj_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $clj_version_path"
    cd "$clj_version_path"

    log "lein clean"
    lein clean

    if [ -d "$clj_version_path/resources" ]
    then
        clean_json_resources "$clj_version_path/resources"
    fi

    CLEAN_LASTEXITCODE=$?

    cd -
}

clean_cpp_version () {
    local base_path="$1"
    local cpp_version_name="$2"

    log "language: C++"
    log "version: $cpp_version_name"

    cpp_version_path="$base_path/cpp/$cpp_version_name"
    log "cpp_version_path: $cpp_version_path"

    if [ ! -d "$cpp_version_path" ]
    then
        log_error "Path not found: $cpp_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $cpp_version_path"
    cd "$cpp_version_path"

    for c in $(find . -name "cmake-build-*" -type d -maxdepth 1)
    do
        log "rm -rf $c"
        rm -rf "$c"

        if [ "$?" -ne 0 ]
        then
            CLEAN_LASTEXITCODE=1
            cd -
            return 1
        fi
    done

    cd -
}

clean_csharp_version () {
    local base_path="$1"
    local cs_version_name="$2"

    log "language: C#"
    log "version: $cs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    cs_version_path="$base_path/csharp/$cs_version_name"
    log "cs_version_path: $cs_version_path"

    if [ ! -d "$cs_version_path" ]
    then
        log_error "Path not found: $cs_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $cs_version_path"
    cd "$cs_version_path"

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    log "dotnet clean -v minimal"
    dotnet clean -v minimal

    PROJECT_PREFIX=""

    if [ "$cs_version_name" == 'csfind' ]
    then
        PROJECT_PREFIX="CsFind"
    elif [ "$cs_version_name" == 'cssearch' ]
    then
        PROJECT_PREFIX="CsSearch"
    else
        log_error "Unknown C# version name: $cs_version_name"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    for p in $(find "$cs_version_path" -name "${PROJECT_PREFIX}*" -type d -maxdepth 1)
    do
        if [ -d "$p" ]
        then
            log "rm -rf $p/bin"
            rm -rf "$p/bin"

            log "rm -rf $p/obj"
            rm -rf "$p/obj"
        fi
    done

    if [ -d "$cs_version_path/${PROJECT_PREFIX}Lib/Resources" ]
    then
        clean_json_resources "$cs_version_path/${PROJECT_PREFIX}Lib/Resources"
    fi

    if [ -d "$cs_version_path/${PROJECT_PREFIX}Tests/Resources" ]
    then
        clean_test_resources "$cs_version_path/${PROJECT_PREFIX}Tests/Resources"
    fi

    cd -
}

clean_dart_version () {
    local base_path="$1"
    local dart_version_name="$2"

    log "language: dart"
    log "version: $dart_version_name"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    dart_version_path="$base_path/dart/$dart_version_name"
    log "dart_version_path: $dart_version_path"

    if [ ! -d "$dart_version_path" ]
    then
        log_error "Path not found: $dart_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $dart_version_path"
    cd "$dart_version_path"

    # pub cache repair is apparently the closest thing to clean for dart
    # but unfortunately it's pretty slow
    log "dart pub cache repair"
    dart pub cache repair

    if [ -n "$LOCKFILE" -a -f "pubspec.lock" ]
    then
        log "rm pubspec.lock"
        rm -f pubspec.lock
    fi

    cd -
}

clean_elixir_version () {
    local base_path="$1"
    local ex_version_name="$2"

    log "language: elixir"
    log "version: $ex_version_name"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    ex_version_path="$base_path/elixir/$ex_version_name"
    log "ex_version_path: $ex_version_path"

    if [ ! -d "$ex_version_path" ]
    then
        log_error "Path not found: $ex_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $ex_version_path"
    cd "$ex_version_path"

    log "mix clean"
    mix clean

    if [ -n "$LOCKFILE" -a -f "mix.lock" ]
    then
        log "rm mix.lock"
        rm -f mix.lock
    fi

    cd -
}

clean_fsharp_version () {
    local base_path="$1"
    local fs_version_name="$2"

    log "language: F#"
    log "version: $fs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    fs_version_path="$base_path/fsharp/$fs_version_name"
    log "fs_version_path: $fs_version_path"

    if [ ! -d "$fs_version_path" ]
    then
        log_error "Path not found: $fs_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $fs_version_path"
    cd "$fs_version_path"

    # Verbosity levels: q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]
    log "dotnet clean -v minimal"
    dotnet clean -v minimal

    PROJECT_PREFIX=""

    if [ "$fs_version_name" == 'fsfind' ]
    then
        PROJECT_PREFIX="FsFind"
    elif [ "$fs_version_name" == 'fssearch' ]
    then
        PROJECT_PREFIX="FsSearch"
    else
        log_error "Unknown F# version name: $fs_version_name"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    for p in $(find "$fs_version_path" -name "${PROJECT_PREFIX}*" -type d -maxdepth 1)
    do
        if [ -d "$p" ]
        then
            log "rm -rf $p/bin"
            rm -rf "$p/bin"

            log "rm -rf $p/obj"
            rm -rf "$p/obj"
        fi
    done

    if [ -d "$fs_version_path/${PROJECT_PREFIX}Lib/Resources" ]
    then
        clean_json_resources "$fs_version_path/${PROJECT_PREFIX}Lib/Resources"
    fi

    if [ -d "$fs_version_path/${PROJECT_PREFIX}Tests/Resources" ]
    then
        clean_test_resources "$fs_version_path/${PROJECT_PREFIX}Tests/Resources"
    fi

    cd -
}

clean_go_version () {
    local base_path="$1"
    local go_version_name="$2"

    log "language: go"
    log "version: $go_version_name"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    go_version_path="$base_path/go/$go_version_name"
    log "go_version_path: $go_version_path"

    if [ ! -d "$go_version_path" ]
    then
        log_error "Path not found: $go_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $go_version_path"
    cd "$go_version_path"

    log "go clean"
    go clean

    CLEAN_LASTEXITCODE=$?

    cd -
}

clean_groovy_version () {
    local base_path="$1"
    local groovy_version_name="$2"

    log "language: groovy"
    log "version: $groovy_version_name"

    groovy_version_path="$base_path/groovy/$groovy_version_name"
    log "groovy_version_path: $groovy_version_path"

    if [ ! -d "$groovy_version_path" ]
    then
        log_error "Path not found: $groovy_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
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
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    if [ -d "$groovy_version_path/lib/src/main/resources" ]
    then
        clean_json_resources "$groovy_version_path/lib/src/main/resources"
    fi

    if [ -d "$groovy_version_path/lib/src/test/resources" ]
    then
        clean_test_resources "$groovy_version_path/lib/src/test/resources"
    fi

    cd -
}

clean_haskell_version () {
    local base_path="$1"
    local hs_version_name="$2"

    log "language: haskell"
    log "version: $hs_version_name"

    # ensure stack is installed
    if [ -z "$(which stack)" ]
    then
        log_error "You need to install stack"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    hs_version_path="$base_path/haskell/$hs_version_name"
    log "hs_version_path: $hs_version_path"

    if [ ! -d "$hs_version_path" ]
    then
        log_error "Path not found: $hs_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $hs_version_path"
    cd "$hs_version_path"

    log "stack clean"
    stack clean

    if [ "$?" -ne 0 ]
    then
        CLEAN_LASTEXITCODE=1
        cd -
        return 1
    fi

    if [ -d "$hs_version_path/data" ]
    then
        clean_json_resources "$hs_version_path/data"
    fi

    if [ -n "$LOCKFILE" -a -f "stack.yaml.lock" ]
    then
        log "rm stack.yaml.lock"
        rm -f stack.yaml.lock
    fi

    cd -
}

clean_java_version () {
    local base_path="$1"
    local java_version_name="$2"

    log "language: java"
    log "version: $java_version_name"

    java_version_path="$base_path/java/$java_version_name"
    log "java_version_path: $java_version_path"

    if [ ! -d "$java_version_path" ]
    then
        log_error "Path not found: $java_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
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
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    if [ -d "$java_version_path/lib/src/main/resources" ]
    then
        clean_json_resources "$java_version_path/lib/src/main/resources"
    fi

    if [ -d "$java_version_path/lib/src/test/resources" ]
    then
        clean_test_resources "$java_version_path/lib/src/test/resources"
    fi

    cd -
}

clean_javascript_version () {
    local base_path="$1"
    local js_version_name="$2"

    log "language: javascript"
    log "version: $js_version_name"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    js_version_path="$base_path/javascript/$js_version_name"
    log "js_version_path: $js_version_path"

    if [ ! -d "$js_version_path" ]
    then
        log_error "Path not found: $js_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $js_version_path"
    cd "$js_version_path"

    log "npm run clean"
    npm run clean

    if [ "$?" -ne 0 ]
    then
        CLEAN_LASTEXITCODE=1
        cd -
        return 1
    fi

    if [ -d "$js_version_path/data" ]
    then
        clean_json_resources "$js_version_path/data"
    fi

    if [ -n "$LOCKFILE" -a -f "package-lock.json" ]
    then
        log "rm package-lock.json"
        rm -f package-lock.json
    fi

    cd -
}

clean_kotlin_version () {
    local base_path="$1"
    local kt_version_name="$2"

    log "language: kotlin"
    log "version: $kt_version_name"

    kt_version_path="$base_path/kotlin/$kt_version_name"
    log "kt_version_path: $kt_version_path"

    if [ ! -d "$kt_version_path" ]
    then
        log_error "Path not found: $kt_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
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
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "$GRADLE --warning-mode all clean"
    "$GRADLE" --warning-mode all clean

    if [ "$?" -ne 0 ]
    then
        CLEAN_LASTEXITCODE=1
        cd -
        return 1
    fi

    if [ -d "$kt_version_path/lib/src/main/resources" ]
    then
        clean_json_resources "$kt_version_path/lib/src/main/resources"
    fi

    if [ -d "$kt_version_path/lib/src/test/resources" ]
    then
        clean_test_resources "$kt_version_path/lib/src/test/resources"
    fi

    cd -
}

clean_ocaml_version () {
    echo
    hdr "clean_ml_version"

    # TODO: probably want to delete the _build directory

    return 0
}

clean_objc_version () {
    local base_path="$1"
    local objc_version_name="$2"

    log "language: objc"
    log "version: $objc_version_name"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    objc_version_path="$base_path/objc/$objc_version_name"
    log "objc_version_path: $objc_version_path"

    if [ ! -d "$objc_version_path" ]
    then
        log_error "Path not found: $objc_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $objc_version_path"
    cd "$objc_version_path"

    log "swift package clean"
    swift package clean

    CLEAN_LASTEXITCODE=$?

    cd -
}

clean_perl_version () {
    local base_path="$1"
    local pl_version_name="$2"

    log "language: perl"
    log "version: $pl_version_name"

    pl_version_path="$base_path/perl/$pl_version_name"
    log "pl_version_path: $pl_version_path"

    if [ ! -d "$pl_version_path" ]
    then
        log_error "Path not found: $pl_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    if [ -d "$pl_version_path/share" ]
    then
        clean_json_resources "$pl_version_path/share"
    fi
}

clean_php_version () {
    local base_path="$1"
    local php_version_name="$2"

    log "language: php"
    log "version: $php_version_name"

    php_version_path="$base_path/php/$php_version_name"
    log "php_version_path: $php_version_path"

    if [ ! -d "$php_version_path" ]
    then
        log_error "Path not found: $php_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    if [ -d "$php_version_path/resources" ]
    then
        clean_json_resources "$php_version_path/resources"
    fi

    if [ -n "$LOCKFILE" -a -f "$php_version_path/composer.lock" ]
    then
        log "rm composer.lock"
        rm -f "$php_version_path/composer.lock"
    fi
}

clean_powershell_version () {
    echo
    hdr "clean_powershell_version"
    log "Nothing to do for powershell"
    # TODO: do we want to uninstall?
}

clean_python_version () {
    local base_path="$1"
    local py_version_name="$2"

    log "language: python"
    log "version: $py_version_name"

    py_version_path="$base_path/python/$py_version_name"
    log "py_version_path: $py_version_path"

    if [ ! -d "$py_version_path" ]
    then
        log_error "Path not found: $py_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    if [ -d "$py_version_path/$py_version_name/data" ]
    then
        clean_json_resources "$py_version_path/$py_version_name/data"
    fi
}

clean_ruby_version () {
    local base_path="$1"
    local rb_version_name="$2"

    log "language: ruby"
    log "version: $rb_version_name"

    rb_version_path="$base_path/ruby/$rb_version_name"
    log "rb_version_path: $rb_version_path"

    if [ ! -d "$rb_version_path" ]
    then
        log_error "Path not found: $rb_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    if [ -d "$rb_version_path/data" ]
    then
        clean_json_resources "$rb_version_path/data"
    fi

    if [ -d "$rb_version_path/test/fixtures" ]
    then
        clean_test_resources "$rb_version_path/test/fixtures"
    fi

    if [ -n "$LOCKFILE" -a -f "$rb_version_path/Gemfile.lock" ]
    then
        log "rm Gemfile.lock"
        rm -f "$rb_version_path/Gemfile.lock"
    fi
}

clean_rust_version () {
    local base_path="$1"
    local rs_version_name="$2"

    log "language: rust"
    log "version: $rs_version_name"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    rs_version_path="$base_path/rust/$rs_version_name"
    log "rs_version_path: $rs_version_path"

    if [ ! -d "$rs_version_path" ]
    then
        log_error "Path not found: $rs_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $rs_version_path"
    cd "$rs_version_path"

    log "cargo clean"
    cargo clean

    if [ "$?" -ne 0 ]
    then
        CLEAN_LASTEXITCODE=1
        cd -
        return 1
    fi

    if [ -n "$LOCKFILE" -a -f "Cargo.lock" ]
    then
        log "rm Cargo.lock"
        rm -f Cargo.lock
    fi

    cd -
}

clean_scala_version () {
    local base_path="$1"
    local scala_version_name="$2"

    log "language: scala"
    log "version: $scala_version_name"

    # ensure sbt is installed
    if [ -z "$(which sbt)" ]
    then
        log_error "You need to install sbt"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    # TODO: convert to sbt command

    scala_version_path="$base_path/scala/$scala_version_name"
    log "scala_version_path: $scala_version_path"

    if [ ! -d "$scala_version_path" ]
    then
        log_error "Path not found: $scala_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $scala_version_path"
    cd "$scala_version_path"

    log "sbt clean"
    sbt clean

    if [ "$?" -ne 0 ]
    then
        CLEAN_LASTEXITCODE=1
        cd -
        return 1
    fi

    if [ -d "$scala_version_path/src/main/resources" ]
    then
        clean_json_resources "$scala_version_path/src/main/resources"
    fi

    if [ -d "$scala_version_path/src/test/resources" ]
    then
        clean_test_resources "$scala_version_path/src/test/resources"
    fi

    cd -
}

clean_swift_version () {
    local base_path="$1"
    local swift_version_name="$2"

    log "language: swift"
    log "version: $swift_version_name"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    swift_version_path="$base_path/swift/$swift_version_name"
    log "swift_version_path: $swift_version_path"

    if [ ! -d "$swift_version_path" ]
    then
        log_error "Path not found: $swift_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $swift_version_path"
    cd "$swift_version_path"

    log "swift package clean"
    swift package clean

    CLEAN_LASTEXITCODE=$?

    cd -
}

clean_typescript_version () {
    local base_path="$1"
    local ts_version_name="$2"

    log "language: typescript"
    log "version: $ts_version_name"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    ts_version_path="$base_path/typescript/$ts_version_name"
    log "ts_version_path: $ts_version_path"

    if [ ! -d "$ts_version_path" ]
    then
        log_error "Path not found: $ts_version_path"
        CLEAN_LASTEXITCODE=1
        return 1
    fi

    log "cd $ts_version_path"
    cd "$ts_version_path"

    log "npm run clean"
    npm run clean

    if [ "$?" -ne 0 ]
    then
        CLEAN_LASTEXITCODE=1
        cd -
        return 1
    fi

    if [ -d "$ts_version_path/data" ]
    then
        clean_json_resources "$ts_version_path/data"
    fi

    if [ -n "$LOCKFILE" -a -f "package-lock.json" ]
    then
        log "rm package-lock.json"
        rm -f package-lock.json
    fi

    cd -
}
