#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

CFIND_PATH="$XFIND_PATH/c/cfind"
CONFIGURATION=debug
# CONFIGURATION=release
CMAKE_BUILD_DIR="$CFIND_PATH/cmake-build-$CONFIGURATION"
CFIND_EXE="$CMAKE_BUILD_DIR/cfindapp"

$CFIND_EXE "$@"
