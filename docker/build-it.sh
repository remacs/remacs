#!/bin/sh

if ! -d $i; then
    echo "You must provide the directory which contains the Dockerfile."
    exit 1
fi

if ! -f $i/Dockerfile; then
    echo "$i/Dockerfile does not exist."
    exit 1
fi

docker build -T $(basename $1) $1
