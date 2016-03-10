# vim: set ft=sh:

test_and_run() {
  if which "$1" >/dev/null; then
    $@
  fi
}

xmodmap ~/.xmodmap;

## MACHINE SPECIFIC STUFF =====================================================
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
  xrandr --output VGA1 --auto --rotate left --output HDMI1 --left-of VGA1 --auto --primary --output HDMI2 --right-of VGA1 --auto
  test_and_run remind -z -k'~/.bin/remind_notify %s' ~/.remind/work.rem
fi

## FONTS ======================================================================
if [ -d /usr/share/fonts/local ]; then
  xset fp+ /usr/share/fonts/local;
fi;

if [ -d ~/.fonts ]; then
  xset fp+ ~/.fonts;
fi;

## EVERY MACHINE ==============================================================
test_and_run dunst &
test_and_run emacs --daemon &
test_and_run unclutter -noevents &
test_and_run firefox &

if [ -e ~/.fehbg -a $(which feh) ]; then
  ~/.fehbg
else
  test_and_run nitrogen --restore
fi


if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
  dbus-launch --sh-syntax --exit-with-session
fi

~/.bin/stat_line &
remind -z -k'~/.bin/remind_notify %s' ~/.remind/work.rem &
exec ~/.bin/dwm
