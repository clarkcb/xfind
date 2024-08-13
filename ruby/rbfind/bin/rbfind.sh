#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH="$HOME/src/xfind"
fi

RBFIND_PATH="$XFIND_PATH/ruby/rbfind"

GEMFILE="$RBFIND_PATH/Gemfile"
RBFIND_EXE="$RBFIND_PATH/bin/rbfind.rb"

#ruby $RBFIND_EXE $@
bundle exec --gemfile "$GEMFILE" ruby "$RBFIND_EXE" "$@"
