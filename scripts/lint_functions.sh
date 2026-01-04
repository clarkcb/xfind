#!/bin/bash
################################################################################
#
# lint_functions.sh
#
# Run static code analysis tools
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# source "$DIR/config.sh"
source "$DIR/common.sh"

# Global variable to hold last funtion exit code
LINT_LASTEXITCODE=0

# Keep track of successful and failed lints
SUCCESSFUL_LINTS=()
FAILED_LINTS=()


########################################
# Utility Functions
########################################

print_lint_results () {
    if [ ${#SUCCESSFUL_LINTS[@]} -gt 0 ]
    then
        log "Successful lints (${#SUCCESSFUL_LINTS[@]}): ${SUCCESSFUL_LINTS[*]}"
    else
        log_error "Successful lints: 0"
    fi
    if [ ${#FAILED_LINTS[@]} -gt 0 ]
    then
        log_error "Failed lints (${#FAILED_LINTS[@]}): ${FAILED_LINTS[*]}"
    else
        log "Failed lints: 0"
    fi
}


########################################
# Lint Functions
########################################

lint_bash_version () {
    local base_path="$1"
    local bash_version_name="$2"

    log "language: bash"

    log "not implemented at this time"
}

lint_c_version () {
    local base_path="$1"
    local c_version_name="$2"

    log "language: c"

    log "not implemented at this time"
}

lint_clj_version () {
    local base_path="$1"
    local clj_version_name="$2"

    log "language: clojure"

    # ensure clojure is installed
    if [ -z "$(which clj)" ]
    then
        log_error "You need to install clojure"
        LINT_LASTEXITCODE=1
        return
    fi

    # clj -version output looks like this: Clojure CLI version 1.11.4.1474
    # CLOJURE_VERSION=$(clj -version | head -n 1 | cut -d ' ' -f 3)
    CLOJURE_VERSION=$(clj -version 2>&1)
    log "clojure version: $CLOJURE_VERSION"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        echo "You need to install lein"
        LINT_LASTEXITCODE=1
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION=$(lein version)
    log "lein version: $LEIN_VERSION"

    clj_version_path="$base_path/clojure/$clj_version_name"
    log "clj_version_path: $clj_version_path"

    if [ ! -d "$clj_version_path" ]
    then
        log_error "clj version path not found: $clj_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $clj_version_path"
    cd "$clj_version_path"

    log "Linting cljfind"
    log "lein eastwood"
    lein eastwood

    LINT_LASTEXITCODE=$?

    cd -
}

lint_cpp_version () {
    echo
    hdr "lint_cpp_version"

    log "not implemented at this time"
}

lint_cs_version () {
    echo
    hdr "lint_cs_version"

    log "not implemented at this time"
}

lint_dart_version () {
    local base_path="$1"
    local dart_version_name="$2"

    log "language: dart"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        LINT_LASTEXITCODE=1
        return
    fi

    DART_VERSION=$(dart --version)
    log "$DART_VERSION"

    dart_version_path="$base_path/dart/$dart_version_name"
    log "dart_version_path: $dart_version_path"

    if [ ! -d "$dart_version_path" ]
    then
        log_error "dart version path not found: $dart_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "Linting $dart_version_name"
    log "dart analyze $dart_version_path"
    dart analyze "$dart_version_path"

    LINT_LASTEXITCODE=$?
}

lint_ex_version () {
    local base_path="$1"
    local ex_version_name="$2"

    log "language: elixir"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        LINT_LASTEXITCODE=1
        return
    fi

    ELIXIR_VERSION=$(elixir --version | grep Elixir)
    log "elixir version: $ELIXIR_VERSION"

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        LINT_LASTEXITCODE=1
        return
    fi

    MIX_VERSION=$(mix --version | grep Mix)
    log "mix version: $MIX_VERSION"

    ex_version_path="$base_path/elixir/$ex_version_name"
    log "ex_version_path: $ex_version_path"

    if [ ! -d "$ex_version_path" ]
    then
        log_error "Path not found: $ex_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $ex_version_path"
    cd "$ex_version_path"

    log "Linting $ex_version_name"
    log "mix credo ."
    mix credo .

    LINT_LASTEXITCODE=$?

    cd -
}

lint_fs_version () {
    echo
    hdr "lint_fs_version"

    log "not implemented at this time"
}

lint_go_version () {
    local base_path="$1"
    local go_version_name="$2"

    log "language: go"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        LINT_LASTEXITCODE=1
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
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $go_version_path"
    cd "$go_version_path"

    log "Linting $go_version_name"
    log "go vet ./..."
    go vet ./...

    LINT_LASTEXITCODE=$?

    cd -
}

lint_groovy_version () {
    local base_path="$1"
    local groovy_version_name="$2"

    log "language: groovy"

    GROOVYLINT="npm-groovy-lint"

    if [ -z "$(which $GROOVYLINT)" ]
    then
        log_error "You need to install $GROOVYLINT"
        LINT_LASTEXITCODE=1
        return
    fi

    groovy_version_path="$base_path/groovy/$groovy_version_name"
    log "groovy_version_path: $groovy_version_path"

    if [ ! -d "$groovy_version_path" ]
    then
        log_error "Path not found: $groovy_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $groovy_version_path"
    cd "$groovy_version_path"

    GROOVY_VERSION_APP_PATH="$groovy_version_path/app/src/main/groovy/groovyfind/app"
    GROOVY_VERSION_LIB_PATH="$groovy_version_path/lib/src/main/groovy/groovyfind"

    log "Linting groovyfind"
    log "$GROOVYLINT $GROOVY_VERSION_APP_PATH $GROOVY_VERSION_LIB_PATH"
    "$GROOVYLINT" "$GROOVY_VERSION_APP_PATH" "$GROOVY_VERSION_LIB_PATH"

    LINT_LASTEXITCODE=$?

    cd -
}

lint_hs_version () {
    local base_path="$1"
    local hs_version_name="$2"

    log "language: haskell"

    HLINT="$HOME/.local/bin/hlint"

    if [ ! -f "$HLINT" ]
    then
        log_error "You need to install hlint"
        LINT_LASTEXITCODE=1
        return
    fi

    hs_version_path="$base_path/haskell/$hs_version_name"
    log "hs_version_path: $hs_version_path"

    if [ ! -d "$hs_version_path" ]
    then
        log_error "Path not found: $hs_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "hlint $hs_version_path"
    "$HLINT" "$hs_version_path"

    LINT_LASTEXITCODE=$?
}

lint_java_version () {
    local base_path="$1"
    local java_version_name="$2"

    log "language: java"

    JAVA_PATH="$base_path/java"

    TOOLS_PATH="$JAVA_PATH/tools"
    if [ ! -d "$TOOLS_PATH" ]
    then
        log "mkdir -p $TOOLS_PATH"
        mkdir -p "$TOOLS_PATH"
    fi

    CHECKSTYLE_VERSION="8.41"
    # CHECKSTYLE_VERSION="10.17.0"
    CHECKSTYLE_JAR=$(find "$TOOLS_PATH" -name "checkstyle*.jar" | grep $CHECKSTYLE_VERSION | head -n 1)
    if [ -z "$CHECKSTYLE_JAR" ]
    then
        log "Checkstyle jar not found, downloading"
        # https://github.com/checkstyle/checkstyle/releases/download/checkstyle-10.17.0/checkstyle-10.17.0-all.jar
        URL="https://github.com/checkstyle/checkstyle/releases/download/checkstyle-$CHECKSTYLE_VERSION/checkstyle-$CHECKSTYLE_VERSION-all.jar"
        cd "$TOOLS_PATH"
        log "curl -J -L -O $URL"
        curl -J -L -O "$URL"
        cd -
        CHECKSTYLE_JAR=$(find "$TOOLS_PATH" -name "checkstyle*.jar" | grep $CHECKSTYLE_VERSION | head -n 1)
    fi

    log "CHECKSTYLE_JAR: $CHECKSTYLE_JAR"

    JAVA="$JAVA_HOME/bin/java"

    java_version_path="$base_path/java/$java_version_name"
    log "java_version_path: $java_version_path"

    if [ ! -d "$java_version_path" ]
    then
        log_error "Path not found: $java_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    # CONFIG=$java_version_path/sun_checks.xml
    CONFIG="$java_version_path/google_checks.xml"

    GREPVS=("Javadoc"
            "hides a field"
            "Line is longer than 80 characters"
            "Missing a Javadoc comment"
            "Missing package-info.java file"
            )

    log "Linting $java_version_name"
    FILES=$(find "$java_version_path/src" -name "*.java")
    for f in ${FILES[*]}
    do
        echo
        log "$JAVA -jar $CHECKSTYLE_JAR -c $CONFIG $f"
        output=$("$JAVA" -jar "$CHECKSTYLE_JAR" -c "$CONFIG" "$f")
        # for g in ${GREPVS[*]}
        # do
        #     output=$(echo $output | grep -v $g)
        # done
        echo -e "$output"
    done
}

lint_js_version () {
    local base_path="$1"
    local js_version_name="$2"

    log "language: javascript"

    js_version_path="$base_path/javascript/$js_version_name"
    log "js_version_path: $js_version_path"

    if [ ! -d "$js_version_path" ]
    then
        log_error "Path not found: $js_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    JSSRC_PATH="$js_version_path/src"
    JSHINT="$js_version_path/node_modules/jshint/bin/jshint"

    if [ ! -f "$JSHINT" ]
    then
        cd "$js_version_path"
        npm install jshint
        cd -
    fi

    FILES=$(find "$JSSRC_PATH" -name "*.js")
    for f in ${FILES[*]}
    do
        log "$JSHINT $f"
        "$JSHINT" "$f"
    done
}

lint_kt_version () {
    local base_path="$1"
    local kt_version_name="$2"

    log "language: kotlin"

    if [ -z "$(which ktlint)" ]
    then
        echo "You need to install ktlint"
        LINT_LASTEXITCODE=1
        return
    fi

    kt_version_path="$base_path/kotlin/$kt_version_name"
    log "kt_version_path: $kt_version_path"

    if [ ! -d "$kt_version_path" ]
    then
        log_error "Path not found: $kt_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $kt_version_path"
    cd "$kt_version_path"

    log "ktlint"
    ktlint

    LINT_LASTEXITCODE=$?

    cd -
}

lint_objc_version () {
    echo
    hdr "lint_objc_version"

    log "not implemented at this time"
}

lint_ocaml_version () {
    echo
    hdr "lint_ocaml_version"

    log "not implemented at this time"
}

lint_pl_version () {
    echo
    hdr "lint_pl_version"

    log "not implemented at this time"
}

lint_php_version () {
    local base_path="$1"
    local php_version_name="$2"

    log "language: php"

    php_version_path="$base_path/php/$php_version_name"
    log "php_version_path: $php_version_path"

    if [ ! -d "$php_version_path" ]
    then
        log_error "Path not found: $php_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $php_version_path"
    cd "$php_version_path"

    if [ ! -f "vendor/bin/phpstan" ]
    then
        echo "You need to install phpstan"
        LINT_LASTEXITCODE=1
        return
    fi

    log "vendor/bin/phpstan analyse -l 9 src tests"
    vendor/bin/phpstan analyse -l 9 src tests

    LINT_LASTEXITCODE=$?

    cd -
}

lint_ps1_version () {
    local base_path="$1"
    local ps1_version_name="$2"

    log "language: powershell"

    ps1_version_path="$base_path/powershell/$ps1_version_name"
    log "ps1_version_path: $ps1_version_path"

    if [ ! -d "$ps1_version_path" ]
    then
        log_error "Path not found: $ps1_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $ps1_version_path"
    cd "$ps1_version_path"

    # This is always going to fail because this is a Cmdlet and only available in Powershell,
    # adding here as a reminder for when I create lint.ps1
    if [ ! -f "invoke-scriptanalyzer" ]
    then
        log "You need to install PSScriptAnalyzer"
        log "(NOTE: only available in Powershell)"
        LINT_LASTEXITCODE=1
        return
    fi

    log "Linting $ps1_version_name"

    log "Invoke-ScriptAnalyzer -Path ."
    Invoke-ScriptAnalyzer -Path .

    LINT_LASTEXITCODE=$?

    cd -
}

lint_py_version () {
    local base_path="$1"
    local py_version_name="$2"

    log "language: python"

    LINTER="ruff"
    LINT_CMD="ruff check"

    if [ -z "$(which $LINTER)" ]
    then
        echo "Linter ruff not found, trying pylint"
        LINTER="pylint"
        LINT_CMD="pylint"

        if [ -z "$(which $LINTER)" ]
        then
            echo "You need to install ruff or pylint"
            return
        fi
    fi

    py_version_path="$base_path/python/$py_version_name"
    log "py_version_path: $py_version_path"

    if [ ! -d "$py_version_path" ]
    then
        log_error "Path not found: $py_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    echo "cd $py_version_path"
    cd "$py_version_path"

    log "$LINT_CMD $py_version_name"
    $LINT_CMD $py_version_name

    LINT_LASTEXITCODE=$?

    cd -
}

lint_rb_version () {
    local base_path="$1"
    local rb_version_name="$2"

    log "language: ruby"

    if [ -z "$(which ruby-lint)" ]
    then
        echo "You need to install ruby-lint"
        LINT_LASTEXITCODE=1
        return
    fi

    rb_version_path="$base_path/ruby/$rb_version_name"
    log "rb_version_path: $rb_version_path"

    if [ ! -d "$rb_version_path" ]
    then
        log_error "Path not found: $rb_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    FILES=$(find "$rb_version_path" -name "*.rb")
    for f in ${FILES[*]}
    do
        log "ruby-lint $f"
        ruby-lint "$f" | grep -v 'undefined'
    done
}

lint_rust_version () {
    echo
    hdr "lint_rust_version"

    log "not implemented at this time"
}

lint_scala_version () {
    local base_path="$1"
    local scala_version_name="$2"

    log "language: scala"

    # TOOLS_PATH=$SCALAFIND_PATH/tools
    # if [ ! -d "$TOOLS_PATH" ]
    # then
    #     log "mkdir -p $TOOLS_PATH"
    #     mkdir -p "$TOOLS_PATH"
    # fi

    # SCALASTYLE_JAR=$(find "$TOOLS_PATH" -name "scalastyle*.jar" | head -n 1)
    # if [ -z "$SCALASTYLE_JAR" ]
    # then
    #     log "Scalastyle jar not found, downloading"
    #     # TODO: is it the batch jar or the regular jar that should be used?
    #     # URL="https://repo1.maven.org/maven2/org/scalastyle/scalastyle_2.12/1.0.0/scalastyle_2.12-1.0.0-batch.jar"
    #     URL="https://repo1.maven.org/maven2/org/scalastyle/scalastyle_2.12/1.0.0/scalastyle_2.12-1.0.0.jar"
    #     cd "$TOOLS_PATH"
    #     curl -O "$URL"
    #     cd -
    #     SCALASTYLE_JAR=$(find "$TOOLS_PATH" -name "scalastyle*.jar" | head -n 1)
    # fi

    # CONFIG="$SCALAFIND_PATH/scalastyle_config.xml"

    # log "Linting scalafind"
    # log "java -jar $SCALASTYLE_JAR --config $CONFIG $SCALAFIND_PATH/src/main/scala"
    # java -jar "$SCALASTYLE_JAR" --config "$CONFIG" "$SCALAFIND_PATH/src/main/scala"

    log "not implemented at this time (scalastyle not available for scala 3.x)"
}

lint_swift_version () {
    local base_path="$1"
    local swift_version_name="$2"

    log "language: swift"

    if [ -z "$(which swiftlint)" ]
    then
        echo "You need to install swiftlint"
        LINT_LASTEXITCODE=1
        return
    fi

    swift_version_path="$base_path/swift/$swift_version_name"
    log "swift_version_path: $swift_version_path"

    if [ ! -d "$swift_version_path" ]
    then
        log_error "Path not found: $swift_version_path"
        LINT_LASTEXITCODE=1
        return
    fi

    log "cd $swift_version_path/Sources"
    cd "$swift_version_path/Sources"

    log "swiftlint"
    swiftlint
    
    if [ "$?" -ne 0 ]
    then
        LINT_LASTEXITCODE=1
        cd -
        return
    fi

    cd -

    log "Linting $swift_version_name Tests"
    log "cd $swift_version_path/Tests; swiftlint; cd -"
    cd "$swift_version_path/Tests"; swiftlint; cd -

    LINT_LASTEXITCODE=$?
}

lint_ts_version () {
    echo
    hdr "lint_ts_version"

    log "Not supported at this time"
}
