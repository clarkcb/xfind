#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

JAVAFIND_PATH="$XFIND_PATH/java/javafind"
JAVAFIND_APP_JAR="$JAVAFIND_PATH/app/build/libs/app.jar"

JAVA_HOME_17=$(/usr/libexec/java_home -v17)

JAVA_HOME="$JAVA_HOME_17" java -jar "$JAVAFIND_APP_JAR" "$@"
