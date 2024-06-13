#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

CLJFIND_PATH="$XFIND_PATH/clojure/cljfind"
CLJFIND_VERSION="0.1.0-SNAPSHOT"
CLJFIND_JAR="$CLJFIND_PATH/target/uberjar/cljfind-$CLJFIND_VERSION-standalone.jar"

#java -jar "$CLJFIND_JAR" "$@"
java -cp "$CLJFIND_JAR" cljfind.cljfind$_main "$@"
