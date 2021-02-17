#!/bin/sh

XFIND_PATH=$HOME/src/xfind
SWIFT_PATH=$XFIND_PATH/swift
SWIFTFIND_PATH=$SWIFT_PATH/swiftfind

# Debug exe location (when compiling from within Xcode)
#SWIFTFINDEXE=$HOME/Library/Developer/Xcode/DerivedData/swiftfind-fnxmcvhlrjapoaeeqzmwblpnnffc/Build/Products/Debug/swiftfind

# Release exe location (when compiling from command line (scripts/build.sh swift))
SWIFTFINDEXE=$SWIFTFIND_PATH/.build/release/swiftfindApp

$SWIFTFINDEXE $@
