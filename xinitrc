## MACHINE SPECIFIC STUFF =====================================================
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
    # == WORK ==
    #xrandr --output VGA1 --auto --right-of HDMI1;
    #xrandr --output HDMI1 --primary --rotate left;
    xrandr --output VGA1 --primary --rotate left --output HDMI1 --auto --right-of VGA1 --output HDMI2 --auto --right-of HDMI1
    remind -z -k'~/.bin/remind_notify "%s"' ~/.remind/work.rem &
fi

## FONTS ======================================================================
if [ -d /usr/share/fonts/local ]; then
    xset fp+ /usr/share/fonts/local;
fi; 

if [ -d $HOME/.fonts ]; then
    xset fp+ $HOME/.fonts;
fi;

## EVERY MACHINE HURR =========================================================
xmodmap ~/.xmodmap;
xautolock -time 3 -locker sxlock &
nitrogen --restore
unclutter -noevents &
urxvtd -f -q -o
firefox &

if [ $(which compton) ]; then 
    compton -cCGfF -o 0.38 -O 200 -I 200 -t 0 -l 0 -r 3 -D2 -m 0.88 &
fi;

if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    dbus-launch --sh-syntax --exit-with-session
fi

~/.bin/stat_line &
exec ~/.bin/dwm 1>&2 2>/tmp/dwm.log
