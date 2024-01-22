#!/usr/bin/env sh

killall -q waybar
while pgrep --uid $UID -x waybar >/dev/null; do sleep 1; done

trap "killall -q waybar" EXIT

CONFIG_FILES="$HOME/.config/waybar/config $HOME/.config/waybar/style.css"
while true; do
    waybar >/dev/null 2>&1 &
    inotifywait -e create,modify $CONFIG_FILES
    killall -q waybar
    while pgrep --uid $UID -x waybar >/dev/null; do sleep 1; done
done
