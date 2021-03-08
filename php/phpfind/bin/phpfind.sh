#!/bin/sh

if [ -z "$XFIND_PATH" ]
then
    XFIND_PATH=$HOME/src/xfind
fi

PHPFIND_PATH=$XFIND_PATH/php/phpfind

PHPFIND_EXE=$PHPFIND_PATH/bin/phpfind.php

php $PHPFIND_EXE $@
