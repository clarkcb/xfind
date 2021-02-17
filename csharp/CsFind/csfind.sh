#!/bin/bash

SCRIPTPATH=$( readlink "${BASH_SOURCE[0]}" )
SCRIPTDIR=$( dirname "$SCRIPTPATH" )
source "$SCRIPTDIR/../../scripts/config.sh" 

CSFIND_PATH=$CSHARP_PATH/CsFind
# CONFIGURATION=Debug
CONFIGURATION=Release
CSFINDEXE=$CSFIND_PATH/CsFind/bin/$CONFIGURATION/netcoreapp3.1/CsFind.dll

dotnet $CSFINDEXE $@
