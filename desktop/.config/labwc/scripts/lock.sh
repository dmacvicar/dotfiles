#!/bin/sh
SUFFIX="-lock-bg-${USER}"
LOCK_BACKGROUND="$(mktemp)${SUFFIX}.png"
grim "${LOCK_BACKGROUND}" && convert -filter Gaussian -resize 20% -blur 0x2.5 -resize 500% "${LOCK_BACKGROUND}" "${LOCK_BACKGROUND}"
swaylock -f -e -i "${LOCK_BACKGROUND}"
shred -u "${LOCK_BACKGROUND}"
