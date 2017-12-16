#!/bin/sh

for i in *; do
    if [ "$i" != "scalable" ]; then
        mkdir -p $i/apps
        convert -resize $i scalable/apps/emacs.svg $i/apps/emacs.png
    fi
done
