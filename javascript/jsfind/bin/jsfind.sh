#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

JSFIND_PATH="$XFIND_PATH/javascript/jsfind"

node "$JSFIND_PATH/dist/jsfind.js" "$@"
