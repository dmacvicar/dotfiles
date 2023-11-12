#!/bin/bash
album_art=$(mpris-ctl "%art_url")
if [[ -z $album_art ]]
then
   # spotify is dead, we should die to.
   exit
fi
curl -s  "${album_art}" --output "/tmp/cover.jpeg"
echo "/tmp/cover.jpeg"
