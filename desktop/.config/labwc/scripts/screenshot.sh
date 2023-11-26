#!/bin/sh
NOW=$(date +"%Y-%m-%d-%H%M%S")
grim -g "$(slurp )" -t png $HOME/Pictures/Screenshots/screenshot-$NOW.png
swappy -f $HOME/Pictures/Screenshots/screenshot-$NOW.png
