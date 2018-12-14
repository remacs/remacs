#!/bin/sh

if [ ! -d "$1" ]; then
    echo "You must provide the directory which contains the Dockerfile."
    exit 1
fi

if ! [ -f $1/Dockerfile ]; then
    echo "$1/Dockerfile does not exist."
    exit 1
fi

docker build --tag $(basename $1) $1
