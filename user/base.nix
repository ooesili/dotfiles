{ pkgs, config }:

with pkgs;

let
  mergedConfig = lib.recursiveUpdate {
    xmodmap.enable = true;
    xmodmap.swapAltSuper = true;
  } config;

  dotfiles = callPackage ./dotfiles.nix { config = mergedConfig; };
  pythonPackages = py-pkgs: with py-pkgs; [
    virtualenv
  ];
  python = python3.withPackages pythonPackages;

in [
  alacritty
  capnproto
  cfssl
  coreutils
  direnv
  discord
  dotfiles
  easytag
  elmPackages.elm
  elmPackages.elm-format
  feh
  ffmpeg
  firefox
  fzf
  ghc
  gimp
  go_1_11
  graphviz
  groff
  htop
  httpie
  hydrogen
  i3
  i3lock
  i3status
  imagemagick
  ipfs
  jack2Full
  jq
  keepassx
  leiningen
  lua
  mpc_cli
  mpv
  mupdf
  ncdu
  ncmpcpp
  openjdk8
  p7zip
  posix_man_pages
  pv
  python
  qjackctl
  ranger
  reflex
  ripgrep
  rofi
  ruby_2_5
  rustup
  scrot
  shellcheck
  signal-desktop
  socat
  spotify
  sqlite
  stack
  supercollider
  tdesktop
  tmux
  unclutter-xfixes
  unixtools.xxd
  vagrant
  wireguard
  xautolock
  xorg.xev
  xorg.xsetroot
  xsel
  zip
]
