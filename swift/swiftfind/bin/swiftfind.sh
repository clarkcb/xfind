#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

SWIFTFIND_PATH=$XFIND_PATH/swift/swiftfind

# Debug exe location (when compiling from within Xcode)
#SWIFTFIND_EXE=$HOME/Library/Developer/Xcode/DerivedData/swiftfind-fnxmcvhlrjapoaeeqzmwblpnnffc/Build/Products/Debug/swiftfind

# Release exe location (when compiling from command line (scripts/build.sh swift))
SWIFTFIND_EXE=$SWIFTFIND_PATH/.build/release/swiftfindApp

$SWIFTFIND_EXE "$@"
