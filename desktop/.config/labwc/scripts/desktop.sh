BACKGROUND_COLOR=$([ "$USER" = "work" ] && echo "#4a3a8a" || echo "#008899")
# secondary monitor
swaybg --output "LG Electronics LG HDR QHD 105NTSU2K566" -i ~/.wallpaper -c "$BACKGROUND_COLOR" >/dev/null 2>&1 &

# main monitor
pcmanfm-qt --desktop >/dev/null 2>&1 &
