#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

RSFIND_PATH=$XFIND_PATH/rust/rsfind
# PROFILE=debug
PROFILE=release

$RSFIND_PATH/target/$PROFILE/rsfind "$@"
