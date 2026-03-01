#!/bin/bash
################################################################################
#
# update_functions.sh
#
# Update functions for xfind or xsearch language versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# source "$DIR/config.sh"
source "$DIR/common.sh"

# Global variable to hold last function exit code
UPDATE_LASTEXITCODE=0

# Keep track of successful and failed updates and report at the end
SUCCESSFUL_UPDATES=()
FAILED_UPDATES=()


########################################
# Utility Functions
########################################

print_update_results () {
    if [ ${#SUCCESSFUL_UPDATES[@]} -gt 0 ]
    then
        log "Successful updates (${#SUCCESSFUL_UPDATES[@]}): ${SUCCESSFUL_UPDATES[*]}"
    else
        log_error "Successful updates: 0"
    fi
    if [ ${#FAILED_UPDATES[@]} -gt 0 ]
    then
        log_error "Failed updates (${#FAILED_UPDATES[@]}): ${FAILED_UPDATES[*]}"
    else
        log "Failed updates: 0"
    fi
}


########################################
# Update Functions
########################################

update_bash_version () {
    local base_path="$1"
    local bash_version_name="$2"

    log "language: bash"
    log "version: $bash_version_name"

    # ensure bash is installed
    if [ -z "$(which bash)" ]
    then
        log_error "You need to install bash"
        UPDATE_LASTEXITCODE=1
        return
    fi

    BASH_VERSION=$(bash --version | head -n 1)
    log "bash version: $BASH_VERSION"

    bash_version_path="$base_path/bash/$bash_version_name"
    log "bash_version_path: $bash_version_path"

    if [ ! -d "$bash_version_path" ]
    then
        log_error "Path not found: $bash_version_path"
        UPDATE_LASTEXITCODE=1
        return
    fi

    DEPS=(awk find grep jq sed sort)
    for d in ${DEPS[*]}
    do
        if [ -z "$(which $d)" ]
        then
            log_error "You need to install $d"
            UPDATE_LASTEXITCODE=1
            return
        fi
    done

    JQ_VERSION=$(jq --version)
    log "jq version: $JQ_VERSION"

    UPDATE_LASTEXITCODE=$?
}

update_c_version () {
    local base_path="$1"
    local c_version_name="$2"

    log "language: C"
    log "version: $c_version_name"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        UPDATE_LASTEXITCODE=1
        return
    fi

    CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
    log "cmake version: $CMAKE_VERSION"

    # TODO: figure out how to check for new dependency versions

    c_version_path="$base_path/c/$c_version_name"
    log "c_version_path: $c_version_path"

    if [ ! -d "$c_version_path" ]
    then
        log_error "Path not found: $c_version_path"
        UPDATE_LASTEXITCODE=1
        return
    fi

    UPDATE_LASTEXITCODE=$?
}

update_clojure_version () {
    local base_path="$1"
    local clj_version_name="$2"

    log "language: clojure"
    log "version: $clj_version_name"

    # ensure clojure is installed
    if [ -z "$(which clj)" ]
    then
        log_error "You need to install clojure"
        UPDATE_LASTEXITCODE=1
        return
    fi

    CLOJURE_VERSION=$(clj -version 2>&1)
    log "clojure version: $CLOJURE_VERSION"

    # ensure lein is installed
    if [ -z "$(which lein)" ]
    then
        log_error "You need to install lein"
        UPDATE_LASTEXITCODE=1
        return
    fi

    # lein version output looks like this: Leiningen 2.9.7 on Java 11.0.24 OpenJDK 64-Bit Server VM
    LEIN_VERSION=$(lein version)
    log "lein version: $LEIN_VERSION"

    # TODO: figure out how to check for new dependency versions

    clj_version_path="$base_path/clojure/$clj_version_name"
    log "clj_version_path: $clj_version_path"

    if [ ! -d "$clj_version_path" ]
    then
        log_error "Path not found: $clj_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    UPDATE_LASTEXITCODE=$?
}

update_cpp_version () {
    local base_path="$1"
    local cpp_version_name="$2"

    log "language: C++"
    log "version: $cpp_version_name"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        UPDATE_LASTEXITCODE=1
        return
    fi

    CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
    log "cmake version: $CMAKE_VERSION"

    # TODO: figure out how to check for new dependency versions

    cpp_version_path="$base_path/cpp/$cpp_version_name"
    log "cpp_version_path: $cpp_version_path"

    if [ ! -d "$cpp_version_path" ]
    then
        log_error "Path not found: $cpp_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    UPDATE_LASTEXITCODE=$?
}

update_csharp_version () {
    local base_path="$1"
    local cs_version_name="$2"

    log "language: C#"
    log "version: $cs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        UPDATE_LASTEXITCODE=1
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # ensure nuget is installed
    if [ -z "$(which nuget)" ]
    then
        log_error "You need to install nuget"
        UPDATE_LASTEXITCODE=1
        return
    fi

    NUGET_VERSION=$(nuget 2>&1 | head -n 1 | awk '{print $3}')
    log "nuget version: $NUGET_VERSION"

    cs_version_path="$base_path/csharp/$cs_version_name"
    log "cs_version_path: $cs_version_path"

    if [ ! -d "$cs_version_path" ]
    then
        log_error "Path not found: $cs_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    dotnet list package --format json | jq -r '
        .projects[]
        | .frameworks[]?
        | select(has("topLevelPackages"))
        | .topLevelPackages[]
        | "\(.id) \(.requestedVersion) \(.resolvedVersion)"
      ' | while IFS= read -r line; do
        # echo "line: $line"
        PACKAGE=$(echo "$line" | awk '{print $1}')
        REQUESTED=$(echo "$line" | awk '{print $2}')
        RESOLVED=$(echo "$line" | awk '{print $3}')
        if [ "$REQUESTED" != "$RESOLVED" ]
        then
            log_error "Package \"$PACKAGE\" requested ($REQUESTED) != resolved ($RESOLVED)"
            UPDATE_LASTEXITCODE=1
            return
        else
            log "Package \"$PACKAGE\" requested == resolved: $RESOLVED"
        fi
    done

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_dart_version () {
    local base_path="$1"
    local dart_version_name="$2"

    log "language: dart"
    log "version: $dart_version_name"

    # ensure dart is installed
    if [ -z "$(which dart)" ]
    then
        log_error "You need to install dart"
        UPDATE_LASTEXITCODE=1
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

    dart pub outdated --json | jq -r '
        .packages[]
        | "\(.package) \(.current.version) \(.upgradable.version) \(.resolvable.version) \(.latest.version)"
      ' | while IFS= read -r line; do
        # echo "line: $line"
        PACKAGE=$(echo "$line" | awk '{print $1}')
        CURRENT=$(echo "$line" | awk '{print $2}')
        UPGRADABLE=$(echo "$line" | awk '{print $3}')
        RESOLVABLE=$(echo "$line" | awk '{print $3}')
        # LATEST=$(echo "$line" | awk '{print $4}')
        if [ "$CURRENT" == "$UPGRADABLE" -a "$UPGRADABLE" == "$RESOLVABLE" ]
        then
            log "Package \"$PACKAGE\" at latest resolvable version: $RESOLVABLE"
        else
            log_error "Package \"$PACKAGE\" current version ($CURRENT) != resolvable version ($RESOLVABLE)"
            UPDATE_LASTEXITCODE=1
            return
        fi
    done

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_elixir_version () {
    local base_path="$1"
    local ex_version_name="$2"

    log "language: elixir"
    log "version: $ex_version_name"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        UPDATE_LASTEXITCODE=1
        return
    fi

    ELIXIR_VERSION=$(elixir --version | grep Elixir)
    log "elixir version: $ELIXIR_VERSION"

    # ensure mix is installed
    if [ -z "$(which mix)" ]
    then
        log_error "You need to install mix"
        UPDATE_LASTEXITCODE=1
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

    # update dependencies
    log "Updating $ex_version_name"
    log "mix deps.update --all"
    mix deps.update --all

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_fsharp_version () {
    local base_path="$1"
    local ex_version_name="$2"

    log "language: F#"
    log "version: $fs_version_name"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        UPDATE_LASTEXITCODE=1
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # ensure nuget is installed
    if [ -z "$(which nuget)" ]
    then
        log_error "You need to install nuget"
        UPDATE_LASTEXITCODE=1
        return
    fi

    NUGET_VERSION=$(nuget 2>&1 | head -n 1 | awk '{print $3}')
    log "nuget version: $NUGET_VERSION"

    fs_version_path="$base_path/fsharp/$fs_version_name"
    log "fs_version_path: $fs_version_path"

    if [ ! -d "$fs_version_path" ]
    then
        log_error "Path not found: $fs_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $fs_version_path"
    cd "$fs_version_path"

    dotnet list package --format json | jq -r '
        .projects[]
        | .frameworks[]?
        | select(has("topLevelPackages"))
        | .topLevelPackages[]
        | "\(.id) \(.requestedVersion) \(.resolvedVersion)"
      ' | while IFS= read -r line; do
        # echo "line: $line"
        PACKAGE=$(echo "$line" | awk '{print $1}')
        REQUESTED=$(echo "$line" | awk '{print $2}')
        RESOLVED=$(echo "$line" | awk '{print $3}')
        # echo "PACKAGE: $PACKAGE"
        # echo "REQUESTED: $REQUESTED"
        # echo "RESOLVED: $RESOLVED"
        if [ "$REQUESTED" != "$RESOLVED" ]
        then
            log_error "Package \"$PACKAGE\" requested ($REQUESTED) != resolved ($RESOLVED)"
            UPDATE_LASTEXITCODE=1
        else
            log "Package \"$PACKAGE\" requested == resolved: $RESOLVED"
        fi
    done

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_go_version () {
    local base_path="$1"
    local go_version_name="$2"

    log "language: go"
    log "version: $go_version_name"

    # ensure go is installed
    if [ -z "$(which go)" ]
    then
        log_error "You need to install go"
        UPDATE_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $go_version_path"
    cd "$go_version_path"

    # update dependencies
    log "Updating $go_version_name"
    log "go get -t -u ./..."
    go get -t -u ./...

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_groovy_version () {
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
        UPDATE_LASTEXITCODE=1
        cd -
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    GRADLE_GROOVY_VERSION=$(echo $GRADLE_OUTPUT | grep '^Groovy' | awk '{print $2}')
    log "Gradle Groovy version: $GRADLE_GROOVY_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    log "Checking for dependency updates"
    log "$GRADLE checkForDependencyUpdates"
    "$GRADLE" checkForDependencyUpdates

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_haskell_version () {
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
        UPDATE_LASTEXITCODE=1
        return
    fi

    STACK_VERSION=$(stack --version)
    log "stack version: $STACK_VERSION"

    hs_version_path="$base_path/haskell/$hs_version_name"
    log "hs_version_path: $hs_version_path"

    if [ ! -d "$hs_version_path" ]
    then
        log_error "Path not found: $hs_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $hs_version_path"
    cd "$hs_version_path"

    # These update stack, but not the dependencies
    log "Updating stack"
    log "stack update"
    stack update

    log "stack upgrade"
    stack upgrade

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_java_version () {
    local base_path="$1"
    local java_version_name="$2"

    log "language: java"
    log "version: $java_version_name"

    # ensure java is installed
    if [ -z "$(which java)" ]
    then
        log_error "You need to install java"
        UPDATE_LASTEXITCODE=1
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
        UPDATE_LASTEXITCODE=1
        cd -
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    GRADLE_GROOVY_VERSION=$(echo $GRADLE_OUTPUT | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $GRADLE_GROOVY_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    log "Checking for dependency updates"
    log "$GRADLE checkForDependencyUpdates"
    "$GRADLE" checkForDependencyUpdates

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_javascript_version () {
    local base_path="$1"
    local js_version_name="$2"

    log "language: javascript"
    log "version: $js_version_name"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node"
        UPDATE_LASTEXITCODE=1
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        UPDATE_LASTEXITCODE=1
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

    log "cd $js_version_path"
    cd "$js_version_path"

    log "npm outdated --json"
    npm outdated --json | jq -r '
        to_entries[] | "\(.key) \(.value.current) \(.value.wanted) \(.value.latest)"
      ' | while IFS= read -r line; do
        # echo "line: $line"
        PACKAGE=$(echo "$line" | awk '{print $1}')
        CURRENT=$(echo "$line" | awk '{print $2}')
        WANTED=$(echo "$line" | awk '{print $3}')
        LATEST=$(echo "$line" | awk '{print $4}')
        if [ "$CURRENT" != "$WANTED" ]
        then
            log_error "$PACKAGE current version ($CURRENT) != wanted version ($WANTED) (latest: $LATEST)"
            UPDATE_LASTEXITCODE=1
            return
        else
            log "$PACKAGE current version == wanted: $WANTED (latest: $LATEST)"
        fi
    done

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_kotlin_version () {
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
        UPDATE_LASTEXITCODE=1
        return
    fi

    GRADLE_OUTPUT=$($GRADLE --version)

    GRADLE_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Gradle' | awk '{print $2}')
    log "$GRADLE version: $GRADLE_VERSION"

    KOTLIN_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Kotlin' | awk '{print $2}')
    log "Kotlin version: $KOTLIN_VERSION"

    JVM_VERSION=$(echo "$GRADLE_OUTPUT" | grep '^Launcher' | awk '{print $3}')
    log "JVM version: $JVM_VERSION"

    log "Checking for dependency updates"
    log "$GRADLE checkForDependencyUpdates"
    "$GRADLE" checkForDependencyUpdates

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_objc_version () {
    local base_path="$1"
    local objc_version_name="$2"

    log "language: objc"
    log "version: $objc_version_name"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        UPDATE_LASTEXITCODE=1
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

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

    log "swift package show-dependencies"
    swift package show-dependencies

    log "swift package update"
    swift package update

    UPDATE_LASTEXITCODE=$?

    cd -
}

# update_ml_version () {
#     echo
#     hdr "update_ml_version"
#
#     cd "$MLFIND_PATH"
#
#     log "Unit-testing mlfind"
#     ./unittest.sh
#
#     cd -
# }

update_perl_version () {
    local base_path="$1"
    local pl_version_name="$2"

    log "language: perl"
    log "version: $pl_version_name"

    # ensure perl is installed
    if [ -z "$(which perl)" ]
    then
        log_error "You need to install perl"
        UPDATE_LASTEXITCODE=1
        return
    fi

    PERL_VERSION="$(perl -e 'print $^V' | grep '^v5')"
    if [ -z $PERL_VERSION ]
    then
        log_error "A 5.x version of perl is required"
        UPDATE_LASTEXITCODE=1
        return
    fi

    log "perl version: $PERL_VERSION"

    # TODO: figure out how to check for new dependency versions

    pl_version_path="$base_path/perl/$pl_version_name"
    log "pl_version_path: $pl_version_path"

    if [ ! -d "$pl_version_path" ]
    then
        log_error "Path not found: $pl_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    UPDATE_LASTEXITCODE=$?
}

update_php_version () {
    local base_path="$1"
    local php_version_name="$2"

    log "language: php"
    log "version: $php_version_name"

    # ensure php is installed
    if [ -z "$(which php)" ]
    then
        log_error "You need to install PHP"
        UPDATE_LASTEXITCODE=1
        return
    fi

    PHP_VERSION=$(php -v | grep '^PHP [78]')
    if [ -z "$PHP_VERSION" ]
    then
        log_error "A version of PHP >= 7.x is required"
        UPDATE_LASTEXITCODE=1
        return
    fi
    log "php version: $PHP_VERSION"

    # ensure composer is installed
    if [ -z "$(which composer)" ]
    then
        log_error "You need to install PHP"
        UPDATE_LASTEXITCODE=1
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

    log "cd $php_version_path"
    cd "$php_version_path"

    composer outdated --direct --format=json | jq -r '
        [.installed[]]
        # | "\(.name) \(.version) \(.latest) \(.'latest-status')"
        | map(.name + " " + .version + " " + .latest + " " + .["latest-status"])[]
      ' | while IFS= read -r line; do
        # echo "line: $line"
        PACKAGE=$(echo "$line" | awk '{print $1}')
        VERSION=$(echo "$line" | awk '{print $2}')
        LATEST=$(echo "$line" | awk '{print $3}')
        STATUS=$(echo "$line" | awk '{print $4}')
        if [ "$VERSION" != "$LATEST" ]
        then
            if [ "$STATUS" != "update-possible" ]
            then
                log_error "Package \"$PACKAGE\" installed ($VERSION) != latest ($LATEST)"
                UPDATE_LASTEXITCODE=1
                return
            else
                log "Package \"$PACKAGE\" version upgrade possible: $VERSION --> $LATEST"
            fi
        else
            log "Package \"$PACKAGE\" installed version == latest: $LATEST"
        fi
    done

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_powershell_version () {
    local base_path="$1"
    local ps1_version_name="$2"

    log "language: powershell"
    log "version: $ps1_version_name"

    # ensure pwsh is installed
    if [ -z "$(which pwsh)" ]
    then
        log_error "You need to install powershell"
        UPDATE_LASTEXITCODE=1
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

    # TODO: figure out how to check for new dependency versions

    UPDATE_LASTEXITCODE=$?
}

update_python_version () {
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

    VENV_PATH="$py_version_path/venv"
    PYTHON="$VENV_PATH/bin/python"
    export PYTHONPATH="$PYTHON_PATH"

    if [ ! -d "$VENV_PATH" ]
    then
        log_error "venv path not found, you probably need to run the python build (./build.sh python)"
        UPDATE_LASTEXITCODE=1
        return
    fi

    echo "cd $py_version_path"
    cd "$py_version_path"

    # activate the virtualenv
    log "source $VENV_PATH/bin/activate"
    source "$VENV_PATH/bin/activate"

    log "pip list --outdated"
    pip list --outdated

    # deactivate the virtualenv
    log "deactivate"
    deactivate

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_ruby_version () {
    local base_path="$1"
    local rb_version_name="$2"

    log "language: ruby"
    log "version: $rb_version_name"

    # ensure ruby3.x+ is installed
    if [ -z "$(which ruby)" ]
    then
        log_error "You need to install ruby"
        UPDATE_LASTEXITCODE=1
        return
    fi

    RUBY_VERSION="$(ruby -v 2>&1 | grep '^ruby 3')"
    if [ -z "$RUBY_VERSION" ]
    then
        log_error "A version of ruby >= 3.x is required"
        UPDATE_LASTEXITCODE=1
        return
    fi

    rb_version_path="$base_path/ruby/$rb_version_name"
    log "rb_version_path: $rb_version_path"

    if [ ! -d "$rb_version_path" ]
    then
        log_error "Path not found: $rb_version_path"
        BUILD_LASTEXITCODE=1
        return
    fi

    log "ruby version: $RUBY_VERSION"

    # ensure bundler is installed
    if [ -z "$(which bundle)" ]
    then
        log_error "You need to install bundler: https://bundler.io/"
        UPDATE_LASTEXITCODE=1
        return
    fi

    echo "cd $rb_version_path"
    cd "$rb_version_path"

    log "bundle update --all"
    bundle update --all

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_rust_version () {
    local base_path="$1"
    local rs_version_name="$2"

    log "language: rust"
    log "version: $rs_version_name"

    # ensure rust is installed
    if [ -z "$(which rustc)" ]
    then
        log_error "You need to install rust"
        UPDATE_LASTEXITCODE=1
        return
    fi

    RUST_VERSION=$(rustc --version)
    log "rustc version: $RUST_VERSION"

    # ensure cargo is installed
    if [ -z "$(which cargo)" ]
    then
        log_error "You need to install cargo"
        UPDATE_LASTEXITCODE=1
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

    # TODO: figure out how to check for new dependency versions

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_scala_version () {
    local base_path="$1"
    local scala_version_name="$2"

    log "language: scala"
    log "version: $scala_version_name"

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
        UPDATE_LASTEXITCODE=1
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
        BUILD_LASTEXITCODE=1
        return
    fi

    log "cd $scala_version_path"
    cd "$scala_version_path"

    log "sbt dependencyUpdates"
    sbt dependencyUpdates

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_swift_version () {
    local base_path="$1"
    local swift_version_name="$2"

    log "language: swift"
    log "version: $swift_version_name"

    # ensure swift is installed
    if [ -z "$(which swift)" ]
    then
        log_error "You need to install swift"
        UPDATE_LASTEXITCODE=1
        return
    fi

    SWIFT_VERSION=$(swift --version 2>&1 | grep Swift)
    log "swift version: $SWIFT_VERSION"

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

    log "swift package show-dependencies"
    swift package show-dependencies

    log "swift package update"
    swift package update

    UPDATE_LASTEXITCODE=$?

    cd -
}

update_typescript_version () {
    local base_path="$1"
    local ts_version_name="$2"

    log "language: typescript"
    log "version: $ts_version_name"

    # if node is installed, display version
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node"
        UPDATE_LASTEXITCODE=1
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

    # ensure npm is installed
    if [ -z "$(which npm)" ]
    then
        log_error "You need to install npm"
        UPDATE_LASTEXITCODE=1
        return
    fi

    NPM_VERSION=$(npm --version)
    log "npm version: $NPM_VERSION"

    ts_version_path="$base_path/typescript/$ts_version_name"
    log "ts_version_path: $ts_version_path"

    log "cd $ts_version_path"
    cd "$ts_version_path"

    log "npm outdated --json"
    npm outdated --json | jq -r '
        to_entries[] | "\(.key) \(.value.current) \(.value.wanted) \(.value.latest)"
      ' | while IFS= read -r line; do
        # echo "line: $line"
        PACKAGE=$(echo "$line" | awk '{print $1}')
        CURRENT=$(echo "$line" | awk '{print $2}')
        WANTED=$(echo "$line" | awk '{print $3}')
        LATEST=$(echo "$line" | awk '{print $4}')
        if [ "$CURRENT" != "$WANTED" ]
        then
            log_error "$PACKAGE current version ($CURRENT) != wanted version ($WANTED) (latest: $LATEST)"
            UPDATE_LASTEXITCODE=1
            return
        else
            log "$PACKAGE current version == wanted: $WANTED (latest: $LATEST)"
        fi
    done

    UPDATE_LASTEXITCODE=$?

    cd -
}
