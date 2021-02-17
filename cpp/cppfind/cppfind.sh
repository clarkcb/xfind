#!/bin/bash

SCRIPTPATH=$(readlink "${BASH_SOURCE[0]}")
SCRIPTDIR=$(dirname "$SCRIPTPATH")

CONFIGURATION=debug
#CONFIGURATION=release
CMAKE_BUILD_DIR=$SCRIPTDIR/cmake-build-$CONFIGURATION
CPPFINDEXE=$CMAKE_BUILD_DIR/cppfind

$CPPFINDEXE "$@"