#!/bin/sh

for i in *; do
    if [ -d "$i" -a "$i" != "scalable" ]; then
        mkdir -p $i/apps
        convert -background none -resize $i scalable/apps/emacs.svg $i/apps/emacs.png
    fi
done
