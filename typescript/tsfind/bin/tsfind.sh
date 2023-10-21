#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

TSFIND_PATH="$XFIND_PATH/typescript/tsfind"

node "$TSFIND_PATH/dist/src/tsfind.js" "$@"
