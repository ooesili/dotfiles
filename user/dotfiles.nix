{ config, writeText, writeScript, lib, stdenvNoCC, makeWrapper, alacritty, fzf, ncmpcpp, neovim, tmux, i3, i3lock, rofi, bash, xorg, ... }:

let
  inherit (lib.lists) last foldl;
  inherit (lib.strings) join removeSuffix;

  neovimPluginRefs = [{
    url = https://github.com/w0rp/ale.git;
    rev = "399a0d3c988381d2436d066e1fe74ef688947f28";
  }{
    url = https://github.com/jiangmiao/auto-pairs.git;
    rev = "f0019fc6423e7ce7bbd01d196a7e027077687fda";
  }{
    url = https://github.com/chriskempson/base16-vim.git;
    rev = "fcce6bce6a2f4b14eea7ea388031c0aa65e4b67d";
  }{
    url = https://github.com/junegunn/fzf.vim.git;
    rev = "f39c92b7ce58669e3b598479131d27093347f4c3";
  }{
    url = https://github.com/chr4/nginx.vim.git;
    rev = "bb003eb1ae5ce16618a071b146c01b324c76fc9e";
  }{
    url = https://github.com/rust-lang/rust.vim.git;
    rev = "2fa74427456a68e9e90f542567f851df50d48a8c";
  }{
    url = https://github.com/edkolev/tmuxline.vim.git;
    rev = "3f9030402aa552a06a350d2adadcbc7ba3469518";
  }{
    url = https://github.com/SirVer/ultisnips.git;
    rev = "6fdc3647f72e0a1f321ea6bd092ecd01f7c187ba";
  }{
    url = https://github.com/tpope/vim-abolish.git;
    rev = "b6a8b49e2173ba5a1b34d00e68e0ed8addac3ebd";
  }{
    url = https://github.com/vim-airline/vim-airline.git;
    rev = "c7fb175d3565159699885653767214a6aa583ea4";
  }{
    url = https://github.com/vim-airline/vim-airline-themes.git;
    rev = "6e798f9030d0853d484078043ddbb41e611ab7a6";
  }{
    url = https://github.com/ntpeters/vim-better-whitespace.git;
    rev = "70a38fa9683e8cd0635264dd1b69c6ccbee4e3e7";
  }{
    url = https://github.com/tpope/vim-commentary.git;
    rev = "141d9d32a9fb58fe474fcc89cd7221eb2dd57b3a";
  }{
    url = https://github.com/tpope/vim-endwise.git;
    rev = "21db2f87983fbf5dca5c07f6330ede2d202fa4d4";
  }{
    url = https://github.com/tpope/vim-eunuch.git;
    rev = "7d1223bfac6009eb26f042e2acf042510ae435bf";
  }{
    url = https://github.com/tpope/vim-fireplace.git;
    rev = "1ef0f0726cadd96547a5f79103b66339f170da02";
  }{
    url = https://github.com/tpope/vim-fugitive.git;
    rev = "81deb6333aeca6e4a266346b0f02945f95dad4d5";
  }{
    url = https://github.com/fatih/vim-go.git;
    rev = "40ffc87ba3eabf299c50ace52be39c1573d0976a";
  }{
    url = https://github.com/fatih/vim-hclfmt.git;
    rev = "155a26611b7358b4b2379d4f3e17c094a1ff38bd";
  }{
    url = https://github.com/LnL7/vim-nix.git;
    rev = "e9abff9a0f4d594e360a5216c4e8f9ed3bcae2c0";
  }{
    url = https://github.com/tpope/vim-repeat.git;
    rev = "43d2678fa59d068c815d8298331c195e850ff5a7";
  }{
    url = https://github.com/honza/vim-snippets.git;
    rev = "1143432afdb3a97b606b081700eead5b4f499d4d";
  }{
    url = https://github.com/tpope/vim-surround.git;
    rev = "597068870b8f093a8b2d11536c62ff31222ee8d0";
  }{
    url = https://github.com/hashivim/vim-terraform.git;
    rev = "4e91b8c3a73fb9ecbf159fb5ca24ed6f39fad4f9";
  }{
    url = https://github.com/cespare/vim-toml.git;
    rev = "85ba8277a6e331a56fce920d62bfdacce5bc5a80";
  }{
    url = https://github.com/tpope/vim-unimpaired.git;
    rev = "d6325994b3c16ce36fd494c47dae4dab8d21a3da";
  }{
    url = https://github.com/junegunn/fzf.git;
    rev = "390b49653b441c958b82a0f78d9923aef4c1d9a2";
  }{
    url = https://github.com/cstrahan/vim-capnp.git;
    rev = "98fcb039b97dbc978a964e36f2681e926b72524d";
  }{
    url = https://github.com/ElmCast/elm-vim.git;
    rev = "e51e2e43ad617c26205a84453481d3ac152c8fec";
  }{
    url = https://github.com/leafgarland/typescript-vim;
    rev = "0e9d92eead2df21abe342c4341c55536dd36b0af";
  }{
    url = https://github.com/Quramy/tsuquyomi;
    rev = "a386d98189a4afe63f23a9174115467c6792aac6";
  }{
    url = https://github.com/pangloss/vim-javascript;
    rev = "7b978de215d864cf6138275e237a5c1d055556a1";
  }{
    url = https://github.com/mxw/vim-jsx;
    rev = "ffc0bfd9da15d0fce02d117b843f718160f7ad27";
  }];

  mkNeovimPlugin = plugin: stdenvNoCC.mkDerivation {
    name =
      let
        basename = path: last (builtins.split "/" path);
        pluginName = removeSuffix ".git" (basename plugin.url);
        version = builtins.substring 0 8 plugin.rev;
      in "neovim-plugin-${pluginName}-${version}";
    src = builtins.fetchGit plugin;

    # don't move doc to share/doc
    forceShare = [ "man" "info" ];

    phases = [ "installPhase" "fixupPhase" ];
    installPhase = "cp -R $src $out";
  };

  vimPathogen = stdenvNoCC.mkDerivation {
    name = "vim-pathogen";
    src = builtins.fetchGit {
      url = https://github.com/tpope/vim-pathogen.git;
      rev = "06da921608b971fb47603671bcafdb2843992eb3";
    };

    phases = [ "installPhase" "fixupPhase" ];
    installPhase = "cp $src/autoload/pathogen.vim $out";
  };

  neovimPlugins = map mkNeovimPlugin neovimPluginRefs;

  lowestPriority = pkgs:
    let
      min = x: y: if x < y then x else y;
      priorities = map (pkg: pkg.meta.priority or 0) pkgs;
    in
      foldl min 0 priorities;

  ohMyZsh = stdenvNoCC.mkDerivation {
    name = "oh-my-zsh";
    src = builtins.fetchGit {
      url = https://github.com/robbyrussell/oh-my-zsh.git;
      rev = "035d78120cb41297068967d3205a23bee22b9543";
    };

    phases = [ "installPhase" "fixupPhase" ];
    installPhase = "cp -p -R $src $out";
  };

  fzfZsh = stdenvNoCC.mkDerivation {
    name = "fzf.zsh";

    phases = [ "installPhase" "fixupPhase" ];
    installPhase = ''
      mkdir -p $out
      cp -R $src/{doc,plugin} $out
    '';
  };

  rofiThemeBase16 = stdenvNoCC.mkDerivation {
    name = "rofi-theme-base16";
    src = builtins.fetchGit {
      url = https://github.com/0xdec/base16-rofi.git;
      rev = "c56ac76ea80daa2883d91afe580121591473504f";
    };

    phases = [ "installPhase" "fixupPhase" ];
    installPhase = "cp -R $src $out";
  };

  xmodmaprc = writeText "xmodmaprc" ''
    ! caps_lock -> left control
    keycode 66 = Control_L

    ! left control -> (none)
    keycode 37 = NoSymbol

    ! remap modifiers
    clear Control
    clear Lock
    add Control = Control_L Control_R
    add Lock = Caps_Lock

    ! leave these around for xcape
    keycode any = Escape

    ${if config.xmodmap.swapAltSuper then ''
    ! you win this time, Apple
    keycode 133 = Super_L
    keycode 134 = Super_R
    keycode 64 = Alt_L
    keycode 108 = Alt_R
    remove Mod4 = Super_L
    remove Mod4 = Super_R
    remove Mod1 = Alt_L
    remove Mod1 = Alt_R
    add Mod1 = Super_L
    add Mod1 = Super_R
    add Mod4 = Alt_L
    add Mod4 = Alt_R
    '' else ""}
  '';

  lockScript = writeScript "lock-script" ''
    #!${bash}/bin/bash
    set -euo pipefail

    ${i3lock}/bin/i3lock --color=000000
    ${xorg.xset}/bin/xset dpms force off
  '';

  xinitrc = writeScript "xinitrc" ''
    #!${bash}/bin/bash

    # environment variables
    . $out/etc/zprofile

    # startup applications
    unclutter --jitter 5 &

    ${if config.xmodmap.enable then ''
    # custom keybindings
    xmodmap ${xmodmaprc}
    '' else ""}

    # tap left control to send escape
    xcape

    # set keyboard repeat rate
    delay=200 # ms
    rate=30 # Hz
    xset r rate "$delay" "$rate"

    exec i3
  '';

in stdenvNoCC.mkDerivation rec {
  name = "dotfiles";
  src = ./.;
  buildInputs = [
    alacritty
    makeWrapper
    ncmpcpp
    neovim
    ohMyZsh
    rofi
    tmux
  ];
  inherit neovimPlugins;
  outputs = [ "out" ];

  patchPhase = ''
    sed -i 's:@@z\.sh:${ohMyZsh}/plugins/z/z.sh:g' files/nvim/init.vim
    sed -i 's:@@ohMyZsh:${ohMyZsh}:g' files/zshrc
    sed -i 's:@@font-size:${config.alacritty.font.size}:g' files/alacritty.yml
    sed -i 's:@@lockScript:${lockScript}:g' files/i3-config
  '';

  installPhase = ''
    mkdir -p $out/etc

    # alacritty
    cp files/alacritty.yml $out/etc/alacritty.yml
    makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty \
      --add-flags "--config-file $out/etc/alacritty.yml"

    # i3
    mkdir -p $out/etc/i3
    cp files/i3-config $out/etc/i3/config
    makeWrapper ${i3}/bin/i3 $out/bin/i3 \
      --add-flags "-c ~/.nix-profile/etc/i3/config"

    # ncmpcpp
    cp -R files/ncmpcpp $out/etc
    makeWrapper ${ncmpcpp}/bin/ncmpcpp $out/bin/ncmpcpp \
      --add-flags "--config=$out/etc/ncmpcpp/config" \
      --add-flags "--bindings=$out/etc/ncmpcpp/bindings"

    # neovim
    mkdir -p $out/etc/xdg/nvim/autoload
    cp -R files/nvim $out/etc/xdg/
    cp ${vimPathogen} $out/etc/xdg/nvim/autoload/pathogen.vim
    mkdir $out/etc/xdg/nvim/bundle
    ln -s $neovimPlugins $out/etc/xdg/nvim/bundle

    # rofi
    mkdir -p $out/etc/rofi/themes
    cp -R ${rofiThemeBase16}/themes/base16-default-dark.rasi $out/etc/rofi/themes/
    makeWrapper ${rofi}/bin/rofi $out/bin/rofi \
      --add-flags "-theme ~/.nix-profile/etc/rofi/themes/base16-default-dark.rasi"

    # tmux
    cp files/tmux.conf $out/etc/tmux.conf
    makeWrapper ${tmux}/bin/tmux $out/bin/tmux \
      --add-flags "-f $out/etc/tmux.conf"

    # xorg
    ln -s ${xinitrc} $out/etc/xinitrc

    # zsh
    cp files/zprofile $out/etc/zprofile
    cp files/zshrc $out/etc/zshrc
  '';

  meta = {
    priority = (lowestPriority buildInputs) - 1;
  };
}
