#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

SCALAFIND_PATH=$XFIND_PATH/scala/scalafind
SCALA_VERSION=2.13
SCALAFIND_JAR=$(find $SCALAFIND_PATH/target/scala-$SCALA_VERSION -name "scalafind-assembly-*.jar" | head -n 1)

# echo "java -Xms1G -Xmx2G -cp $SCALAFIND_JAR scalafind.FindMain $@"
java -cp "$SCALAFIND_JAR" scalafind.FindMain "$@"
