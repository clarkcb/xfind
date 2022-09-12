#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

DARTFIND_PATH=$XFIND_PATH/dart/dartfind
PACKAGES_PATH=$DARTFIND_PATH/.packages
DARTFIND_EXE=$DARTFIND_PATH/bin/dartfind.dart

# dart --packages="$PACKAGES_PATH" "$DARTFIND_EXE" "$@"
dart "$DARTFIND_EXE" "$@"
