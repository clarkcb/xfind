#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

GROOVYFIND_PATH="$XFIND_PATH/groovy/groovyfind"
GROOVYFIND_APP_JAR="$GROOVYFIND_PATH/app/build/libs/app.jar"

java -jar "$GROOVYFIND_APP_JAR" "$@"
