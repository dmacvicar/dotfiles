displayCount=$((`xrandr -q|grep " connected"|wc -l`))
intDisplay=$(xrandr -q | grep " connected" | grep "LVDS" | awk '{print $1}')
ext1Display=$(xrandr -q | grep " connected" |  grep -P 'HDMI(.*)1' | awk '{print $1}')
ext2Display=$(xrandr -q | grep " connected" |  grep -P 'VGA(.*)1' | awk '{print $1}')

echo "${displayCount} -> Int: ${intDisplay} | Ext1: ${ext1Display} | Ext2: ${ext2Display}"

xrandr --auto

if [ $displayCount -eq 3 ]; then
    echo "Enabling work dock"
    xrandr --output $ext2Display --left-of $ext1Display --output $ext1Display --primary --output $intDisplay --off
elif [ $displayCount -eq 2 ]; then
    echo "Enabling home dock"
    xrandr --output $ext1Display --primary --output $intDisplay --off
elif [ $displayCount -eq 1 ]; then
    echo "Enabling one display $intDisplay"
    xrandr --output $intDisplay --primary
fi

sleep 1
#xset r rate 300 50
