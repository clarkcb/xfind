#!/bin/bash

RSFIND_PATH=/Users/cary/src/xfind/rust/rsfind
# PROFILE=debug
PROFILE=release

$RSFIND_PATH/target/$PROFILE/rsfind "$@"
