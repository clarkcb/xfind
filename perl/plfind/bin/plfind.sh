#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

PLFIND_PATH="$XFIND_PATH/perl/plfind"

PLFIND_EXE="$PLFIND_PATH/bin/plfind.pl"

perl $PLFIND_EXE "$@"
