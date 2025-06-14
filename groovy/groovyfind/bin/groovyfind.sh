#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

GROOVYFIND_PATH="$XFIND_PATH/groovy/groovyfind"
GROOVYFIND_APP_JAR="$GROOVYFIND_PATH/app/build/libs/app.jar"

JAVA_HOME_17=$(/usr/libexec/java_home -v17)

JAVA_HOME="$JAVA_HOME_17" java -jar "$GROOVYFIND_APP_JAR" "$@"
