#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

DARTFIND_PATH="$XFIND_PATH/dart/dartfind"
DARTFIND_EXE="$DARTFIND_PATH/bin/dartfind.exe"

"$DARTFIND_EXE" "$@"
