#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

JAVAFIND_PATH=$XFIND_PATH/java/javafind
JAVAFIND_VERSION="0.1.0-SNAPSHOT"
# JAVAFIND_JAR=$(find $JAVAFIND_PATH/target -name "javafind*.jar" | head -n 1)
JAVAFIND_JAR="$JAVAFIND_PATH/target/javafind-$JAVAFIND_VERSION.jar"

# echo "java -cp $JAVAFIND_JAR javafind.FindMain $@"
java -cp "$JAVAFIND_JAR" javafind.FindMain "$@"
