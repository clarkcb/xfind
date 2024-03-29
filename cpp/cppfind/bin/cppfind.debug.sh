#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

CPPFIND_PATH="$XFIND_PATH/cpp/cppfind"
CONFIGURATION=debug
# CONFIGURATION=release
CMAKE_BUILD_DIR="$CPPFIND_PATH/cmake-build-$CONFIGURATION"
CPPFIND_EXE="$CMAKE_BUILD_DIR/cppfindapp"

$CPPFIND_EXE "$@"
