#!/bin/sh
XSEARCH_PATH=~/src/git/xsearch
CSSEARCH_PATH=$XSEARCH_PATH/csharp/CsSearch
CONFIGURATION=Debug
#CONFIGURATION=Release
CSSEARCHEXE=$CSSEARCH_PATH/CsSearch/bin/$CONFIGURATION/CsSearch.exe
mono $CSSEARCHEXE $@
