# vim: set ft=sh:

test_and_run() {
    if which "$1" >/dev/null; then
        $@
    fi
}

xmodmap ~/.xmodmap;

## MACHINE SPECIFIC STUFF =====================================================
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
    xrandr --output VGA1 --auto --output HDMI1 --left-of VGA1 --auto --primary --output HDMI2 --right-of VGA1 --auto
    test_and_run thunderbird &
fi

## FONTS ======================================================================
if [ -d /usr/share/fonts/local ]; then
    xset fp+ /usr/share/fonts/local;
fi;

if [ -d ~/.fonts ]; then
    xset fp+ ~/.fonts;
fi;

## EVERY MACHINE ==============================================================
test_and_run urxvtd -f -q -o

test_and_run compton -cCGfb --backend glx --paint-on-overlay --glx-no-stencil --vsync opengl-swc -b &
test_and_run xautolock -time 3 -locker sxlock &
test_and_run unclutter -noevents &
if [ -e ~/.fehbg ]; then
    eval $(<~/.fehbg)
else
    test_and_run nitrogen --restore
fi
test_and_run firefox &
if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    dbus-launch --sh-syntax --exit-with-session
fi

~/.bin/stat_line &
exec ~/.bin/dwm
