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

# load Xresources and related files
Xfiles="$HOME/.Xsolarized $HOME/.Xresources $HOME/.Xshading"
for file in $Xfiles; do
    [[ -f $file ]] && xrdb -merge $file
done

# environment variables
[[ -f ~/.zprofile ]] && source ~/.zprofile

# startup applications
urxvtd -q -f -o
unclutter -grab -jitter 5 &
eval $(cat ~/.fehbg)
xsetroot -cursor_name left_ptr
xmodmap ~/.capscape
inithome &

# start window manager
exec xmonad
