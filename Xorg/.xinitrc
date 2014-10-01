#!/bin/sh
#
# ~/.xinitrc
#
# executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# load Xresources
[[ -f "$HOME/.Xresources" ]] && xrdb -merge "$HOME/.Xresources"

# environment variables
[[ -f ~/.zprofile ]] && source ~/.zprofile

# startup applications
unclutter -jitter 5 &
eval $(cat ~/.fehbg)
xsetroot -cursor_name left_ptr
xmodmap ~/.capscape
inithome &

# start window manager
exec herbstluftwm --locked
