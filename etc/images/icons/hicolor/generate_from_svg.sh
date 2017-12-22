#!/bin/sh

# This uses the rsvg-convert tool from the rsvg package
# On Debian systems this is in librsvg2-bin
# On OSX use homebrew and install the librsvg formula

APP=emacs

for i in *; do
    if [ -d "$i" -a "$i" != "scalable" ]; then
        mkdir -p $i/apps

        width=`echo $i|cut -f1 -dx`
        height=`echo $i|cut -f2 -dx`

        rsvg-convert --format png --background-color none \
                     --width $width --height $height \
                     --output $i/apps/${APP}.png scalable/apps/${APP}.svg
    fi
done
