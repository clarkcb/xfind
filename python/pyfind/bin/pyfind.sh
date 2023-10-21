#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

PYFIND_PATH="$XFIND_PATH/python/pyfind"

PYFIND_EXE="$PYFIND_PATH/bin/pyfind.py"

python3 "$PYFIND_EXE" "$@"
