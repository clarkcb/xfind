#!/bin/bash
################################################################################
#
# docker_setup.sh
#
# Runs a docker build to create an image to run xfind inside of
#
################################################################################

########################################
# Configuration
########################################

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
source "$DIR/config.sh"
source "$DIR/common.sh"
DEVCONTAINER_PATH=$XFIND_PATH/.devcontainer
# Unset the following to disable experimental/squash
EXPERIMENTAL=Y
SQUASH="--squash"


########################################
# Build Functions
########################################

build_docker_image () {
    echo
    hdr "build_docker_image"

    cd $DEVCONTAINER_PATH

    log "Building docker image for xfind"
    if [ -n "$EXPERIMENTAL" ] && [ "$EXPERIMENTAL" == "Y" ]
    then
        log "docker build $SQUASH -t xfind $DEVCONTAINER_PATH"
        docker build "$SQUASH" -t xfind .
    else
        log "docker build -t xfind $DEVCONTAINER_PATH"
        docker build -t xfind .
    fi

    cd -
}


########################################
# Main
########################################

build_docker_image
