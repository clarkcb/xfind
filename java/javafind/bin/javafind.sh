#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

JAVAFIND_PATH=$XFIND_PATH/java/javafind
JAVAFIND_JAR=$(find $JAVAFIND_PATH/target -name "javafind*.jar" | head -n 1)

java -cp "$JAVAFIND_JAR" javafind.FindMain "$@"
