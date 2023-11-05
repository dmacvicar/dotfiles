#!/bin/sh
NOW=$(date +"%Y-%m-%d-%H%M%S")
grim -g "$(slurp )" -t png $HOME/Pictures/grim-$NOW.png
ksnip $HOME/Pictures/grim-$NOW.png
