#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

SWIFTFIND_PATH=$XFIND_PATH/swift/swiftfind
SWIFTFIND_EXE=$SWIFTFIND_PATH/.build/debug/swiftfindApp

$SWIFTFIND_EXE "$@"
