#!/bin/bash
################################################################################
#
# unittest.sh
#
# Runs unit tests for specified language version of xsearch, or all versions
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"


########################################
# Unit Test Functions
########################################

unittest_clojure () {
    echo
    log "unittest_clojure"
    CLJSEARCH_PATH=$XSEARCH_PATH/clojure/cljsearch
    RESOURCES_PATH=$CLJSEARCH_PATH/resources

    # Test with lein
    log "Unit-testing cljsearch"
    cd $CLJSEARCH_PATH
    log "lein test"
    lein test
    cd -
}

unittest_csharp () {
    echo
    log "unittest_csharp"
    CSHARP_PATH=$XSEARCH_PATH/csharp
    CSSEARCHTESTS_PATH=$CSHARP_PATH/CsSearch/CsSearchTests
    CONFIGURATION=Debug
    #CONFIGURATION=Release

    # run a mono xbuild
    log "Unit-testing cssearch"
    log "mono $CSSEARCHTESTS_PATH/bin/$CONFIGURATION/CsSearchTests.exe"
    mono $CSSEARCHTESTS_PATH/bin/$CONFIGURATION/CsSearchTests.exe
}

unittest_fsharp () {
    echo
    log "unittest_fsharp"
    FSHARP_PATH=$XSEARCH_PATH/fsharp
    RESOURCES_PATH=$FSHARP_PATH/FsSearch/Resources

    # run a mono xbuild
    log "Unit-testing fssearch"
}

unittest_go () {
    echo
    log "unittest_go"
    export GOPATH=$XSEARCH_PATH/go
    export GOSRC_PATH=$GOPATH/src/elocale.com/clarkcb/xsearch

    # now build gosearch
    log "Unit-testing gosearch"
    log "go test"
    cd $GOSRC_PATH; go test; cd -
}

unittest_haskell () {
    echo
    log "unittest_haskell"
    HASKELL_PATH=$XSEARCH_PATH/haskell
    HSSEARCH_PATH=$HASKELL_PATH/hssearch

    # build with cabal
    log "Unit-testing hssearch"
    log "cabal test"
    cd $HSSEARCH_PATH/; cabal test; cd -
}

unittest_java () {
    echo
    log "unittest_java"
    JAVA_PATH=$XSEARCH_PATH/java
    JAVASEARCH_PATH=$JAVA_PATH/javasearch

    # run tests via maven
    log "Unit-testing javasearch"
    log "mvn -f $JAVASEARCH_PATH/pom.xml test"
    mvn -f $JAVASEARCH_PATH/pom.xml test
}

unittest_node () {
    echo
    log "unittest_node"
    NODE_PATH=$XSEARCH_PATH/node
    NODESEARCH_PATH=$NODE_PATH/nodesearch
    TESTS_PATH=$NODESEARCH_PATH/tests
    NODEUNIT_PATH=$NODESEARCH_PATH/node_modules/nodeunit
    NODEUNIT=$NODEUNIT_PATH/bin/nodeunit

    if [ ! -d $NODEUNIT_PATH ]; then
        log "nodeunit not installed, installing"
        cd $NODESEARCH_PATH
        npm install nodeunit
        cd -
    fi

    # run tests via maven
    log "Unit-testing nodesearch"
    log "nodeunit $TESTS_PATH"
    $NODEUNIT $TESTS_PATH
}

unittest_perl () {
    echo
    log "unittest_perl"
    PERL_PATH=$XSEARCH_PATH/perl
    TESTS_PATH=$PERL_PATH/tests

    # run tests using Test::Simple
    log "Unit-testing plsearch"
    FILES=$(find $TESTS_PATH -name "*.pl")
    for f in ${FILES[*]}; do
        log "perl $f"
        perl $f
    done
}

unittest_php () {
    echo
    log "unittest_php"
    PHP_PATH=$XSEARCH_PATH/php
    TESTS_PATH=$PHP_PATH/tests
    PHPUNIT="phpunit --bootstrap $PHP_PATH/phpsearch/autoload.php"

    # run tests with phpunit
    log "Unit-testing phpsearch"
    FILES=$(find $TESTS_PATH -name "*.php")
    for f in ${FILES[*]}; do
        log "phpunit $f"
        $PHPUNIT $f
    done
}

unittest_python () {
    echo
    log "unittest_python"
    PYTHON_PATH=$XSEARCH_PATH/python
    TESTS_PATH=$PYTHON_PATH/tests
    VENV_PATH=$PYTHON_PATH/.env
    PYTHON=$VENV_PATH/bin/python
    export PYTHONPATH=$PYTHON_PATH:$PYTHONPATH

    # activate the virtualenv
    source $PYTHON_PATH/.env/bin/activate

    # Run the individual tests
    log "Unit-testing pysearch"
    FILES=$(find $TESTS_PATH -name "*.py")
    for f in ${FILES[*]}; do
        log "python $f"
        $PYTHON $f
    done

    # deactivate the virtualenv
    deactivate
}

unittest_ruby () {
    echo
    log "unittest_ruby"
    RUBY_PATH=$XSEARCH_PATH/ruby
    TESTS_PATH=$RUBY_PATH/tests

    # Run the individual tests
    log "Unit-testing rbsearch"
    FILES=$(find $TESTS_PATH -name "*.rb")
    for f in ${FILES[*]}; do
        log "ruby $f"
        ruby $f
    done
}

unittest_scala () {
    echo
    log "unittest_scala"
    SCALA_PATH=$XSEARCH_PATH/scala
    SCALASEARCH_PATH=$SCALA_PATH/scalasearch

    # run tests via maven
    log "Unit-testing scalasearch"
    log "mvn -f $SCALASEARCH_PATH/pom.xml test"
    mvn -f $SCALASEARCH_PATH/pom.xml test
}

unittest_all () {
    log "unittest_all"
    
    unittest_clojure

    unittest_csharp

    unittest_fsharp

    unittest_go

    unittest_haskell

    unittest_java

    unittest_node

    unittest_perl

    unittest_php

    unittest_python

    unittest_ruby

    unittest_scala
}


########################################
# Unit-testing main
########################################

if [ $# == 0 ]; then
    ARG="all"
else
    ARG=$1
fi

if [ "$ARG" == "all" ]; then
    unittest_all
elif [ "$ARG" == "clojure" ]; then
    unittest_clojure
elif [ "$ARG" == "csharp" ]; then
    unittest_csharp
elif [ "$ARG" == "fsharp" ]; then
    unittest_fsharp
elif [ "$ARG" == "go" ]; then
    unittest_go
elif [ "$ARG" == "haskell" ]; then
    unittest_haskell
elif [ "$ARG" == "java" ]; then
    unittest_java
elif [ "$ARG" == "node" ]; then
    unittest_node
elif [ "$ARG" == "perl" ]; then
    unittest_perl
elif [ "$ARG" == "php" ]; then
    unittest_php
elif [ "$ARG" == "python" ]; then
    unittest_python
elif [ "$ARG" == "ruby" ]; then
    unittest_ruby
elif [ "$ARG" == "scala" ]; then
    unittest_scala
else
    echo "ERROR: unknown unittest argument: $ARG"
fi

