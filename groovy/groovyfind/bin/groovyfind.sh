#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

GROOVYFIND_PATH="$XFIND_PATH/groovy/groovyfind"
GROOVYFIND_VERSION="0.1.0"
#GROOVYFIND_APP_JAR="$GROOVYFIND_PATH/app/build/libs/app.jar"
GROOVYFIND_JAR="$GROOVYFIND_PATH/build/libs/groovyfind-${GROOVYFIND_VERSION}-SNAPSHOT.jar"

#java -jar "$GROOVYFIND_APP_JAR" "$@"
#echo "java -cp $GROOVYFIND_JAR groovyfind.GroovyFind $@"
java -cp "$GROOVYFIND_JAR" "groovyfind.GroovyFind" "$@"
