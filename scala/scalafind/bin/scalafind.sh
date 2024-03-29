#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

SCALAFIND_PATH="$XFIND_PATH/scala/scalafind"
SCALA_VERSION=3.2.2
SCALAFIND_JAR=$(find "$SCALAFIND_PATH/target/scala-$SCALA_VERSION" -maxdepth 1 -name "scalafind-assembly-*.jar" | head -n 1)

# echo "java -cp \"$SCALAFIND_JAR\" scalafind.FindMain $@"
java -cp "$SCALAFIND_JAR" scalafind.FindMain "$@"
