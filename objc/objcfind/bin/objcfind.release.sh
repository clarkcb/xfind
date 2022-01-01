#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

OBJCFIND_PATH=$XFIND_PATH/objc/objcfind

# Debug exe location (when compiling from within Xcode)
#OBJCFINDEXE=$HOME/Library/Developer/Xcode/DerivedData/objcfind-ahhnhqcmbhdevtgqfhmgnnerqaln/Build/Products/Debug/objcfind

# Release exe location (when compiling from command line (scripts/build.sh objc))
OBJCFIND_EXE=$OBJCFIND_PATH/build/Release/objcfind

$OBJCFIND_EXE $@
