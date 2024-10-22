#!/bin/bash
################################################################################
#
# lint.sh
#
# Run static code analysis tools
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
    echo -e "\nUsage: lint.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}


########################################
# Lint Functions
########################################

lint_c () {
    echo
    hdr "lint_c"

    log "not implemented at this time"
}

lint_clojure () {
    echo
    hdr "lint_clojure"

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

    log "Linting cljfind"
    log "lein eastwood"
    lein eastwood

    cd -
}

lint_cpp () {
    echo
    hdr "lint_cpp"

    log "not implemented at this time"
}

lint_csharp () {
    echo
    hdr "lint_csharp"

    log "not implemented at this time"
}

lint_dart () {
    echo
    hdr "lint_dart"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        return
    fi

    DART_VERSION=$(dart --version)
    log "$DART_VERSION"

    log "Linting dartfind"
    log "dart analyze $DARTFIND_PATH"
    dart analyze "$DARTFIND_PATH"
}

lint_elixir () {
    echo
    hdr "lint_elixir"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        return
    fi

    ELIXIR_VERSION=$(elixir --version | grep Elixir)
    log "elixir version: $ELIXIR_VERSION"

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        return
    fi

    MIX_VERSION=$(mix --version | grep Mix)
    log "mix version: $MIX_VERSION"

    log "Linting exfind"
    log "mix credo $EXFIND_PATH"
    mix credo "$EXFIND_PATH"
}

lint_fsharp () {
    echo
    hdr "lint_fsharp"

    log "not implemented at this time"
}

lint_go () {
    echo
    hdr "lint_go"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        echo "You need to install go"
        return
    fi

    GO_VERSION=$(go version | sed 's/go version //')
    # GO_VERSION=$(go version | head -n 1 | cut -d ' ' -f 3)
    log "go version: $GO_VERSION"

    cd "$GOFIND_PATH"

    log "Linting gofind"
    log "go vet ./..."
    go vet ./...

    cd -
}

lint_groovy () {
    echo
    hdr "lint_groovy"

    GROOVYLINT="npm-groovy-lint"

    if [ -z "$(which $GROOVYLINT)" ]
    then
        log_error "You need to install $GROOVYLINT"
        return
    fi

    GROOVYFIND_APP_PATH="$GROOVYFIND_PATH/app/src/main/groovy/groovyfind/app"
    GROOVYFIND_LIB_PATH="$GROOVYFIND_PATH/lib/src/main/groovy/groovyfind"

    log "Linting groovyfind"
    log "$GROOVYLINT $GROOVYFIND_APP_PATH $GROOVYFIND_LIB_PATH"
    "$GROOVYLINT" "$GROOVYFIND_APP_PATH" "$GROOVYFIND_LIB_PATH"
}

lint_haskell () {
    echo
    hdr "lint_haskell"

    HLINT="$HOME/.local/bin/hlint"

    if [ ! -f "$HLINT" ]
    then
        log_error "You need to install hlint"
        return
    fi

    log "Linting hsfind"
    log "hlint $HSFIND_PATH"
    "$HLINT" "$HSFIND_PATH"
}

lint_java () {
    echo
    hdr "lint_java"

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
    # CONFIG=$JAVAFIND_PATH/sun_checks.xml
    CONFIG="$JAVAFIND_PATH/google_checks.xml"

    GREPVS=("Javadoc"
            "hides a field"
            "Line is longer than 80 characters"
            "Missing a Javadoc comment"
            "Missing package-info.java file"
            )

    log "Linting javafind"
    FILES=$(find "$JAVAFIND_PATH/src" -name "*.java")
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

lint_javascript () {
    echo
    hdr "lint_javascript"

    JSSRC_PATH="$JSFIND_PATH/src"
    JSHINT="$JSFIND_PATH/node_modules/jshint/bin/jshint"

    if [ ! -f "$JSHINT" ]
    then
        cd "$JSFIND_PATH"
        npm install jshint
        cd -
    fi

    log "Linting jsfind"
    FILES=$(find "$JSSRC_PATH" -name "*.js")
    for f in ${FILES[*]}
    do
        log "$JSHINT $f"
        "$JSHINT" "$f"
    done
}

lint_kotlin () {
    echo
    hdr "lint_kotlin"

    if [ -z "$(which ktlint)" ]
    then
        echo "You need to install ktlint"
        return
    fi

    log "Linting ktfind"
    cd "$KTFIND_PATH"
    log "ktlint"
    ktlint
    cd -
}

lint_objc () {
    echo
    hdr "lint_objc"

    log "not implemented at this time"
}

lint_ocaml () {
    echo
    hdr "lint_ocaml"

    log "not implemented at this time"
}

lint_perl () {
    echo
    hdr "lint_perl"

    log "not implemented at this time"
}

lint_php () {
    echo
    hdr "lint_php"

    cd "$PHPFIND_PATH"

    if [ ! -f "vendor/bin/phpstan" ]
    then
        echo "You need to install phpstan"
        return
    fi

    log "Linting phpfind"

    log "vendor/bin/phpstan analyse -l 9 src tests"
    vendor/bin/phpstan analyse -l 9 src tests

    cd -
}

lint_powershell () {
    echo
    hdr "lint_powershell"

    cd "$PS1FIND_PATH"

    # This is always going to fail because this is a Cmdlet and only available in Powershell,
    # adding here as a reminder for when I create lint.ps1
    if [ ! -f "invoke-scriptanalyzer" ]
    then
        echo "You need to install PSScriptAnalyzer"
        echo "(NOTE: only available in Powershell)"
        return
    fi

    log "Linting ps1find"

    log "Invoke-ScriptAnalyzer -Path ."
    Invoke-ScriptAnalyzer -Path .

    cd -
}

lint_python () {
    echo
    hdr "lint_python"

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


    log "Linting pyfind"
    cd "$PYFIND_PATH"
    log "$LINT_CMD pyfind"
    $LINT_CMD pyfind
    cd -
}

lint_ruby () {
    echo
    hdr "lint_ruby"

    if [ -z "$(which ruby-lint)" ]
    then
        echo "You need to install ruby-lint"
        return
    fi

    log "Linting rbfind"
    FILES=$(find "$RBFIND_PATH" -name "*.rb")
    for f in ${FILES[*]}
    do
        log "ruby-lint $f"
        ruby-lint "$f" | grep -v 'undefined'
    done
}

lint_rust () {
    echo
    hdr "lint_rust"

    log "not implemented at this time"
}

lint_scala () {
    echo
    hdr "lint_scala"

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

lint_swift () {
    echo
    hdr "lint_swift"

    if [ -z "$(which swiftlint)" ]
    then
        echo "You need to install swiftlint"
        return
    fi

    log "Linting swiftfind Sources"
    log "cd $SWIFTFIND_PATH/Sources; swiftlint; cd -"
    cd "$SWIFTFIND_PATH/Sources"; swiftlint; cd -

    log "Linting swiftfind Tests"
    log "cd $SWIFTFIND_PATH/Tests; swiftlint; cd -"
    cd "$SWIFTFIND_PATH/Tests"; swiftlint; cd -
}

lint_typescript () {
    echo
    hdr "lint_typescript"

    log "Not supported at this time"
}

lint_all () {
    log "lint_all"
    
    lint_c

    lint_clojure

    lint_cpp

    lint_csharp

    lint_dart

    lint_fsharp

    lint_go

    lint_haskell

    lint_java

    lint_javascript

    lint_kotlin

    lint_objc

    lint_ocaml

    lint_perl

    lint_php

    lint_python

    lint_ruby

    lint_rust

    lint_scala

    lint_swift

    lint_typescript
}


########################################
# Lint Main
########################################
HELP=
LINT_ALL=
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
            LINT_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "LINT_ALL: $LINT_ALL"
log "TARGET_LANGS: ${TARGET_LANGS[*]}"

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$LINT_ALL" ]
then
    lint_all
    exit
fi

if [ ${#TARGET_LANGS[@]} == 0 ]
then
    usage
fi

for TARGET_LANG in ${TARGET_LANGS[*]}
do
    case $TARGET_LANG in
        c)
            lint_c
            ;;
        clj | clojure)
            lint_clojure
            ;;
        cpp)
            lint_cpp
            ;;
        cs | csharp)
            lint_csharp
            ;;
        dart)
            lint_dart
            ;;
        elixir | ex)
            lint_elixir
            ;;
        fs | fsharp)
            lint_fsharp
            ;;
        go)
            lint_go
            ;;
        groovy)
            lint_groovy
            ;;
        haskell | hs)
            lint_haskell
            ;;
        java)
            lint_java
            ;;
        javascript | js)
            lint_javascript
            ;;
        kotlin | kt)
            lint_kotlin
            ;;
        objc)
            lint_objc
            ;;
        # ocaml | ml)
        #     lint_ocaml
        #     ;;
        perl | pl)
            lint_perl
            ;;
        php)
            lint_php
            ;;
        ps1 | powershell)
            lint_powershell
            ;;
        py | python)
            lint_python
            ;;
        rb | ruby)
            lint_ruby
            ;;
        rs | rust)
            lint_rust
            ;;
        scala)
            lint_scala
            ;;
        swift)
            lint_swift
            ;;
        ts | typescript)
            lint_typescript
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done
