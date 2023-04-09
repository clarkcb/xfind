#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

CLJFIND_PATH=$XFIND_PATH/clojure/cljfind
CLJFIND_JAR=$(find $CLJFIND_PATH/target/uberjar -maxdepth 1 -name "cljfind*.jar" | grep standalone | head -n 1)

java -jar "$CLJFIND_JAR" "$@"
