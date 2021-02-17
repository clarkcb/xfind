#!/bin/bash

SCRIPTPATH=$( readlink "${BASH_SOURCE[0]}" )
SCRIPTDIR=$( dirname "$SCRIPTPATH" )
source "$SCRIPTDIR/../../scripts/config.sh" 

FSFIND_PATH=$FSHARP_PATH/FsFind
CONFIGURATION=Debug
#CONFIGURATION=Release
FSFINDEXE=$FSFIND_PATH/FsFind/bin/$CONFIGURATION/netcoreapp3.1/FsFind.dll

dotnet $FSFINDEXE $@
