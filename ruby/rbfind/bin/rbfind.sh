#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

RBFIND_PATH="$XFIND_PATH/ruby/rbfind"

#OLDPWD=`pwd`

cd "$RBFIND_PATH"

RBFIND_EXE="$RBFIND_PATH/bin/rbfind.rb"

#ruby $RBFIND_EXE $@
bundle exec ruby "$RBFIND_EXE" "$@"

#cd "$OLDPWD"
