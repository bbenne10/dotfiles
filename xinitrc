test_and_run() {
    if which "$1" >/dev/null; then
        $@
    fi
}

## MACHINE SPECIFIC STUFF =====================================================
if [ "$HOST" = "CTISL-bbennett-arch-desktop" ]; then
    # == WORK ==
    xrandr --output VGA1 --primary --rotate left --output HDMI1 --auto --right-of VGA1 --output HDMI2 --auto --right-of HDMI1
    test_and_run remind -z -k'~/.bin/remind_notify "%s"' ~/.remind/work.rem &
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
xmodmap ~/.xmodmap;

test_and_run compton -cCGfb --backend glx --paint-on-overlay --glx-no-stencil --vsync opengl-swc -b
test_and_run dunst &
test_and_run xautolock -time 3 -locker sxlock &
test_and_run unclutter -noevents &
test_and_run nitrogen --restore
test_and_run firefox &

~/.bin/stat_line &
exec ~/.bin/dwm 1>&2 2>/tmp/dwm.log
