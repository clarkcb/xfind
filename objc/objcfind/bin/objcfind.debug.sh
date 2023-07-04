#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

OBJCFIND_PATH=$XFIND_PATH/objc/objcfind
OBJCFIND_EXE=$OBJCFIND_PATH/.build/debug/objcfindApp

$OBJCFIND_EXE "$@"
