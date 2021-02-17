#!/bin/bash

SCRIPTPATH=$(readlink "${BASH_SOURCE[0]}")
SCRIPTDIR=$(dirname "$SCRIPTPATH")
PROJECTDIR=$(dirname "$SCRIPTDIR")
PACKAGESPATH=$PROJECTDIR/.packages
DARTFINDPATH=$SCRIPTDIR/dartfind.dart

dart --packages="$PACKAGESPATH" "$DARTFINDPATH" "$@"
