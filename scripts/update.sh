#!/bin/bash
################################################################################
#
# update.sh
#
# Runs checks projects for updates to language and dependencies.
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
    echo -e "\nUsage: update.sh [-h|--help] {\"all\" | lang [lang...]}\n"
    exit
}

print_failed_builds () {
    if [ ${#FAILED_BUILDS[@]} -gt 0 ]
    then
        log_error "Failed updates (${#FAILED_BUILDS[@]}): ${FAILED_BUILDS[*]}"
    else
        log "All updates succeeded"
    fi
}


########################################
# Update Functions
########################################

update_bashfind () {
    echo
    hdr "update_bashfind"

    # ensure bash is installed
    if [ -z "$(which bash)" ]
    then
        log_error "You need to install bash"
        FAILED_BUILDS+=("bashfind")
        return
    fi

    BASH_VERSION=$(bash --version | head -n 1)
    log "bash version: $BASH_VERSION"

    DEPS=(awk find grep jq sed sort)
    for d in ${DEPS[*]}
    do
        if [ -z "$(which $d)" ]
        then
            log_error "You need to install $d"
            FAILED_BUILDS+=("bashfind")
            return
        fi
    done

    JQ_VERSION=$(jq --version)
    log "jq version: $JQ_VERSION"
}

update_cfind () {
    echo
    hdr "update_cfind"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        FAILED_BUILDS+=("cfind")
        return
    fi

    CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
    log "cmake version: $CMAKE_VERSION"

    # TODO: figure out how to check for new dependency versions
}

update_cljfind () {
    echo
    hdr "update_cljfind"

    # ensure clojure is installed
    if [ -z "$(which clojure)" ]
    then
        log_error "You need to install clojure"
        FAILED_BUILDS+=("cljfind")
        return
    fi

    CLOJURE_VERSION=$(clj -version 2>&1)
    log "clojure version: $CLOJURE_VERSION"

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

    # TODO: figure out how to check for new dependency versions
}

update_cppfind () {
    echo
    hdr "update_cppfind"

    # ensure cmake is installed
    if [ -z "$(which cmake)" ]
    then
        log_error "You need to install cmake"
        FAILED_BUILDS+=("cfind")
        return
    fi

    CMAKE_VERSION=$(cmake --version | head -n 1 | cut -d ' ' -f 3)
    log "cmake version: $CMAKE_VERSION"

    # TODO: figure out how to check for new dependency versions
}

update_csfind () {
    echo
    hdr "update_csfind"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("csfind")
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # ensure nuget is installed
    if [ -z "$(which nuget)" ]
    then
        log_error "You need to install nuget"
        FAILED_BUILDS+=("csfind")
        return
    fi

    NUGET_VERSION=$(nuget 2>&1 | head -n 1 | awk '{print $3}')
    log "nuget version: $NUGET_VERSION"

    cd "$CSFIND_PATH"

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
            FAILED_BUILDS+=("csfind")
        else
            log "Package \"$PACKAGE\" requested == resolved: $RESOLVED"
        fi
    done

    cd -
}

update_dartfind () {
    echo
    hdr "update_dartfind"

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
            FAILED_BUILDS+=("dartfind")
        fi
    done

    cd -
}

update_exfind () {
    echo
    hdr "update_exfind"

    # ensure elixir is installed
    if [ -z "$(which elixir)" ]
    then
        log_error "You need to install elixir"
        FAILED_BUILDS+=("exfind")
        return
    fi

    ELIXIR_VERSION=$(elixir --version | grep Elixir)
    log "elixir version: $ELIXIR_VERSION"

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

    # update dependencies
    log "Updating exfind"
    log "mix deps.update --all"
    mix deps.update --all

    cd -
}

update_fsfind () {
    echo
    hdr "update_fsfind"

    # ensure dotnet is installed
    if [ -z "$(which dotnet)" ]
    then
        log_error "You need to install dotnet"
        FAILED_BUILDS+=("csfind")
        return
    fi

    DOTNET_VERSION=$(dotnet --version)
    log "dotnet version: $DOTNET_VERSION"

    # ensure nuget is installed
    if [ -z "$(which nuget)" ]
    then
        log_error "You need to install nuget"
        FAILED_BUILDS+=("csfind")
        return
    fi

    NUGET_VERSION=$(nuget 2>&1 | head -n 1 | awk '{print $3}')
    log "nuget version: $NUGET_VERSION"

    cd "$FSFIND_PATH"

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
            FAILED_BUILDS+=("fsfind")
        else
            log "Package \"$PACKAGE\" requested == resolved: $RESOLVED"
        fi
    done

    cd -
}

update_gofind () {
    echo
    hdr "update_gofind"

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

    cd "$GOFIND_PATH"

    # update dependencies
    log "Updating gofind"
    log "go get -t -u ./..."
    go get -t -u ./...

    cd -
}

update_groovyfind () {
    echo
    hdr "update_groovyfind"

    # if groovy is installed, display version
    if [ -n "$(which groovy)" ]
    then
        GROOVY_VERSION=$(groovy --version)
        log "$GROOVY_VERSION"
    fi

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

    log "Checking for dependency updates"
    log "$GRADLE checkForDependencyUpdates"
    "$GRADLE" checkForDependencyUpdates

    cd -
}

update_hsfind () {
    echo
    hdr "update_hsfind"

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
    
    # These update stack, but not the dependencies
    log "Updating stack"
    log "stack update"
    stack update

    log "stack upgrade"
    stack upgrade

    cd -
}

update_javafind () {
    echo
    hdr "update_javafind"

    # ensure java is installed
    if [ -z "$(which java)" ]
    then
        log_error "You need to install java"
        FAILED_BUILDS+=("javafind")
        return
    fi

    JAVA_VERSION=$(java -version 2>&1 | head -n 1)
    log "java version: $JAVA_VERSION"

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

    cd -
}

update_jsfind () {
    echo
    hdr "update_jsfind"

    # ensure node is installed
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node"
        FAILED_BUILDS+=("jsfind")
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

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
            FAILED_BUILDS+=("jsfind")
            return
        else
            log "$PACKAGE current version == wanted: $WANTED (latest: $LATEST)"
        fi
    done

    cd -
}

update_ktfind () {
    echo
    hdr "update_ktfind"

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

    log "Checking for dependency updates"
    log "$GRADLE checkForDependencyUpdates"
    "$GRADLE" checkForDependencyUpdates

    cd -
}

update_objcfind () {
    echo
    hdr "update_objcfind"

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

    log "swift package show-dependencies"
    swift package show-dependencies

    log "swift package update"
    swift package update

    cd -
}

# update_mlfind () {
#     echo
#     hdr "update_mlfind"
#
#     cd "$MLFIND_PATH"
#
#     log "Unit-testing mlfind"
#     ./unittest.sh
#
#     cd -
# }

update_plfind () {
    echo
    hdr "update_plfind"

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

    # TODO: figure out how to check for new dependency versions
}

update_phpfind () {
    echo
    hdr "update_phpfind"

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

    # ensure composer is installed
    if [ -z "$(which composer)" ]
    then
        log_error "You need to install PHP"
        FAILED_BUILDS+=("phpfind")
        return
    fi

    COMPOSER_VERSION=$(composer --version 2>&1 | grep '^Composer')
    log "composer version: $COMPOSER_VERSION"

    cd "$PHPFIND_PATH"

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
                FAILED_BUILDS+=("phpfind")
                return
            else
                log "Package \"$PACKAGE\" version upgrade possible: $VERSION --> $LATEST"
            fi
        else
            log "Package \"$PACKAGE\" installed version == latest: $LATEST"
        fi
    done

    cd -
}

update_ps1find () {
    echo
    hdr "update_ps1find"

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

    # TODO: figure out how to check for new dependency versions
}

update_pyfind () {
    echo
    hdr "update_pyfind"

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

    log "pip list --outdated"
    pip list --outdated

    # deactivate the virtualenv
    log "deactivate"
    deactivate

    cd -
}

update_rbfind () {
    echo
    hdr "update_rbfind"

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

    # ensure bundler is installed
    if [ -z "$(which bundle)" ]
    then
        log_error "You need to install bundler: https://bundler.io/"
        FAILED_BUILDS+=("rbfind")
        return
    fi

    cd "$RBFIND_PATH"

    log "bundle update"
    bundle update

    cd -
}

update_rsfind () {
    echo
    hdr "update_rsfind"

    # ensure rust is installed
    if [ -z "$(which rustc)" ]
    then
        log_error "You need to install rust"
        FAILED_BUILDS+=("rsfind")
        return
    fi

    RUST_VERSION=$(rustc --version)
    log "rustc version: $RUST_VERSION"

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

    # TODO: figure out how to check for new dependency versions

    cd -
}

update_scalafind () {
    echo
    hdr "update_scalafind"

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

    log "sbt dependencyUpdates"
    sbt dependencyUpdates

    cd -
}

update_swiftfind () {
    echo
    hdr "update_swiftfind"

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

    log "swift package show-dependencies"
    swift package show-dependencies

    log "swift package update"
    swift package update

    cd -
}

update_tsfind () {
    echo
    hdr "update_tsfind"

    # if node is installed, display version
    if [ -z "$(which node)" ]
    then
        log_error "You need to install node"
        FAILED_BUILDS+=("tsfind")
        return
    fi

    NODE_VERSION=$(node --version)
    log "node version: $NODE_VERSION"

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
            FAILED_BUILDS+=("tsfind")
            return
        else
            log "$PACKAGE current version == wanted: $WANTED (latest: $LATEST)"
        fi
    done

    cd -
}

update_all () {
    hdr "update_all"

    update_bashfind

    update_cfind

    update_cljfind

    update_cppfind

    update_csfind

    update_dartfind

    update_exfind

    update_fsfind

    update_gofind

    update_groovyfind

    update_hsfind

    update_javafind

    update_jsfind

    update_ktfind

    update_objcfind

    # update_mlfind

    update_plfind

    update_phpfind

    update_ps1find

    update_pyfind

    update_rbfind

    update_rsfind

    update_scalafind

    update_swiftfind

    update_tsfind
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
UPDATE_ALL=
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
            UPDATE_ALL=yes
            ;;
        *)
            TARGET_LANGS+=($1)
            ;;
    esac
    shift || true
done

# log the settings
log "HELP: $HELP"
log "UPDATE_ALL: $UPDATE_ALL"
if [ ${#TARGET_LANGS[@]} -gt 0 ]
then
    log "TARGET_LANGS (${#TARGET_LANGS[@]}): ${TARGET_LANGS[*]}"
fi

if [ -n "$HELP" ]
then
    usage
fi

if [ -n "$UPDATE_ALL" ]
then
    update_all
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
            update_bashfind
            ;;
        c)
            update_cfind
            ;;
        clj | clojure)
            update_cljfind
            ;;
        cpp)
            update_cppfind
            ;;
        cs | csharp)
            update_csfind
            ;;
        dart)
            update_dartfind
            ;;
        elixir | ex)
            update_exfind
            ;;
        fs | fsharp)
            update_fsfind
            ;;
        go)
            update_gofind
            ;;
        groovy)
            update_groovyfind
            ;;
        haskell | hs)
            update_hsfind
            ;;
        java)
            update_javafind
            ;;
        javascript | js)
            update_jsfind
            ;;
        kotlin | kt)
            update_ktfind
            ;;
        objc)
            update_objcfind
            ;;
        # ocaml | ml)
        #     update_mlfind
        #     ;;
        perl | pl)
            update_plfind
            ;;
        php)
            update_phpfind
            ;;
        ps1 | powershell | pwsh)
            update_ps1find
            ;;
        py | python)
            update_pyfind
            ;;
        rb | ruby)
            update_rbfind
            ;;
        rs | rust)
            update_rsfind
            ;;
        scala)
            update_scalafind
            ;;
        swift)
            update_swiftfind
            ;;
        ts | typescript)
            update_tsfind
            ;;
        *)
            log_error "ERROR: unknown/unsupported language: $TARGET_LANG"
            ;;
    esac
done

print_failed_builds
