#!/bin/sh

XFIND_PATH=$HOME/src/xfind
OBJC_PATH=$XFIND_PATH/objc
OBJCFIND_PATH=$OBJC_PATH/objcfind

# Debug exe location (when compiling from within Xcode)
#OBJCFINDEXE=$HOME/Library/Developer/Xcode/DerivedData/objcfind-ahhnhqcmbhdevtgqfhmgnnerqaln/Build/Products/Debug/objcfind

# Release exe location (when compiling from command line (scripts/build.sh objc))
OBJCFINDEXE=$OBJCFIND_PATH/build/Release/objcfind

$OBJCFINDEXE $@
