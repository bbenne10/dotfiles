# vim: set ft=sh:

test_and_run() {
  if which "$1" >/dev/null; then
    $@
  fi
}

setxkbmap -option caps:escape


## MACHINE SPECIFIC STUFF =====================================================
if [ "$(hostname)" = "CTISL-bbennett-arch-desktop" ]; then
  xrandr --output VGA1 --auto --output HDMI1 --left-of VGA1 --auto --primary --output HDMI2 --right-of VGA1 --auto
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
test_and_run xscreensaver -nosplash &
test_and_run firefox &
test_and_run compton &
test_and_run redshift &

if [ -e ~/.fehbg -a $(which feh) ]; then
  ~/.fehbg
else
  test_and_run nitrogen --restore
fi


if which dbus-launch >/dev/null && test -z "$DBUS_SESSION_BUS_ADDRESS"; then
  dbus-launch --sh-syntax --exit-with-session
fi

if [ -f /etc/X11/xinit/xinitrc.d/50-systemd-user.sh ]; then
 . /etc/X11/xinit/xinitrc.d/50-systemd-user.sh
fi

~/.bin/stat_line &
exec ~/.bin/dwm
# test_and_run albert &
# test_and_run lalcal --width 160 --font "DroidSans" --calfontsize 12 --color "#262626" --format "%m/%d/%y @ %l:%M %p" &
# exec openbox
# exec awesome
