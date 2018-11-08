dotfiles
====================

:warning: :no_entry: :warning: :no_entry: :warning: :no_entry: :warning:
:no_entry: :warning: :no_entry:

I haven't used this repository in a couple years. I've moved onto better things (NixOS). You can find my the current incarnation
of my dotfiles [here on Gitlab](https://gitlab.com/ooesili/dotfiles).

:warning: :no_entry: :warning: :no_entry: :warning: :no_entry: :warning:
:no_entry: :warning: :no_entry:

# Introduction

My configuration files are organized by application. I recommend cloning this
repository into `$HOME/.dotfiles` and using [GNU Stow][1] to symlink the files
into place. I use [Arch Linux][2], so these files will work best with Arch, but
they will probably work just fine on another distribution (with a few tweaks).

# Dependencies and Descriptions

### bin

The scripts in `bin` depend on a variety of different programs. If they don't
work, take a peep in to the code, none of them are complicated. Most of them
are written in Perl, but you'll probably already have it installed, as `git`
depends on it.

### conky

The `conkyrc` file was made to be used with the [conky-cli][3] package (it's
written to be text only).

It should work fine with a version `conky` compiled with all of the normal
options, but you'll need to add some settings to turn of the X features. You'll
more than likely have to change a few things in `conkyr` because your computer
will probably have different names for network devices and different hardware
thermometers than my computer.

### ghc

This directory contains my `ghci` prompt and a nifty wrapper that provides
colour support for `ghci`. It goes without saying that depends on `ghc`. 

The colour wrapper script is based of some script that I found on somewhere,
but I changed quite significantly from the original version. Be warned that
Haskell (and most programming languages) simply cannot be parsed with
non-recursive regular expressions, so the output can sometimes look a little
wonky.

### git

Depends on `git`. Duh.

Definitely check out the aliases. Also note that, if you use this, make sure to
change `user.name` and `user.email`. I don't want anyone out there
impersonating me :)

### gtk

You won't get any use out of this if you don't use any gtk2 applications. The
theme is [Nmix Solarized][4], the icon theme is plain old [Numix][5], and the
cursor theme is [OpenZone][6].

### i3

I use [i3][7] as a window manager, with the built i3bar program as a status
bar. Instead of using `i3-status` for displaying the status bar text, I use
conky. It uses the [Terminess][8] font. It also uses [st][11] as it's terminal
by default.

`Alt-d` runs `firefox`, so if you use something else, make sure to change it.
`Alt-m` and `Alt-Shift-m` run [dmenu][17].

I have the 8th and 9th workspaces configured for use with [non-daw][18] and
[qjackctl][19]. `Alt-n` loads the mixer layout on the 8th workspace, and there
is a wrapper for non-daw in `bin` that loads the layout for the 9th workspace.

### htop

Depends on nothing else.

### ncmpcpp

`ncmpcpp` is a front end for [MPD][9], so you will need to have `mpd`
installed and configured to use this one.

### powerline

This one should be stowed if you are going to use `tmux` or `vim`. It needs the
[Terminess][8] font, and the [powerline][10] package.

### ranger

Depends on nothing else.

### tmux

Depends on the `powerline` directory and [Terminess][8] to work properly, but
it will work (it won't look pretty), without them.

### vim

Optionally depends on the `powerline` directory (python2 support is needed for
this to work). The submodules need to be initialized for vim to properly.

### w3m

Depends on nothing else.

### Xorg

This directory is for starting and configuring the `Xorg` environment. The
`xinitrc` script calls quite a few program, which are listed here:

 * [unclutter][13]
 * [feh][14]
 * [xsetroot][15]
 * [xmodmap][16]
 * The `i3` directory *needs* to be stowed for this work as is.

### zsh

The only things this one really "depends" on are the various programs used in
the `aliases` file. It uses the [grml-zsh][12] system, but it's all bundled up
into a portable format here.


[1]:  https://www.gnu.org/software/stow/
[2]:  https://www.archlinux.org
[3]:  https://aur.archlinux.org/packages/conky-cli/
[4]:  https://aur.archlinux.org/packages/gtk-theme-numix-solarized/
[5]:  https://aur.archlinux.org/packages/numix-icon-theme-git/
[6]:  https://aur.archlinux.org/packages/xcursor-openzone/
[7]:  https://www.archlinux.org/packages/community/x86_64/i3-wm/
[8]:  https://aur.archlinux.org/packages/terminess-powerline-font/
[9]: https://www.archlinux.org/packages/extra/x86_64/mpd/
[10]: https://aur.archlinux.org/packages/python-powerline-git/
[11]: https://aur.archlinux.org/packages/st
[12]: http://grml.org/zsh/
[13]: https://www.archlinux.org/packages/community/x86_64/unclutter/
[14]: https://www.archlinux.org/packages/extra/x86_64/feh/
[15]: https://www.archlinux.org/packages/extra/x86_64/xorg-xsetroot/
[16]: https://www.archlinux.org/packages/extra/x86_64/xorg-xmodmap/
[17]: https://www.archlinux.org/packages/community/x86_64/dmenu/
[18]: https://aur.archlinux.org/packages/non-daw-git
[19]: https://www.archlinux.org/packages/extra/x86_64/qjackctl/
