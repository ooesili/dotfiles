{ config, pkgs, lib, ... }:

let
  cfg = config.dotfiles.desktop;

  alacrittyWrapped = pkgs.callPackage ../../pkgs/alacritty-config {
    config.fontSize = cfg.alacritty.font.size;
  };

  rofiThemeBase16 = builtins.fetchGit {
    name = "base46-rofi-theme";
    url = https://github.com/0xdec/base16-rofi.git;
    rev = "0efb1185eb530c4ee7a33755b399d16b527af706";
  };

  rofiWrapped = with pkgs; runCommand "rofi-config-wrapped" {
    buildInputs = [ makeWrapper ];
  } ''
    makeWrapper ${rofi}/bin/rofi $out/bin/rofi \
      --add-flags "-theme ${rofiThemeBase16}/themes/base16-default-dark.rasi"
  '';

  lockScript = with pkgs; writeScript "lock-script" ''
    #!${bash}/bin/bash
    set -euo pipefail

    ${i3lock}/bin/i3lock --color=000000
    ${xorg.xset}/bin/xset dpms force off
  '';

  xmodmaprc = with pkgs; writeText "xmodmaprc" ''
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

    ${if cfg.xmodmap.swapAltSuper then ''
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

  i3Config = with pkgs; substituteAll {
    name = "i3-config";
    src = ./i3-config;

    alacritty = "${alacrittyWrapped}/bin/alacritty";
    amixer = "${alsaUtils}/bin/amixer";
    feh = "${feh}/bin/feh";
    i3 = i3;
    i3status = "${i3status}/bin/i3status";
    lockScript = lockScript;
    mpc = "${mpc_cli}/bin/mpc";
    rofiWrapped = "${rofiWrapped}/bin/rofi";
    xautolock = "${xautolock}/bin/xautolock";
    xsetroot = "${xorg.xsetroot}/bin/xsetroot";
  };

  xinitrc = with pkgs; writeScript "xinitrc" ''
    #!${bash}/bin/bash

    # environment variables
    . ${cfg.shellProfile}

    # startup applications
    ${unclutter-xfixes}/bin/unclutter --jitter 5 &

    ${if cfg.xmodmap.enable then ''
    # custom keybindings
    ${xorg.xmodmap}/bin/xmodmap ${xmodmaprc}
    '' else ""}

    # tap left control to send escape
    ${xcape}/bin/xcape

    # set keyboard repeat rate
    delay=200 # ms
    rate=30 # Hz
    ${xorg.xset}/bin/xset r rate "$delay" "$rate"

    exec ${i3}/bin/i3 -c ${i3Config}
  '';

in with lib; {
  options = {
    dotfiles.desktop = {
      alacritty.font.size = mkOption {
        default = "10.0";
        description = "Font size of Alacritty terminal windows.";
        type = types.str;
      };

      autoLoginUser = mkOption {
        default = null;
        description = "Automatically login as this user on startup.";
        type = types.nullOr types.str;
      };

      shellProfile = mkOption {
        description = "Shell profile file to source on login.";
        type = types.str;
      };

      xmodmap = {
        enable = mkOption {
          default = true;
          description = "Remap capslock and escape using xmodmap.";
          type = types.bool;
        };

        swapAltSuper = mkOption {
          default = true;
          description = "Swap Alt and Super.";
        };
      };
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      alacritty
      alacrittyWrapped
      scrot
      xorg.xev
      xsel
    ];

    services = {
      geoclue2.enable = true;

      xserver = {
        enable = true;
        desktopManager.default = "none";
        displayManager.lightdm = {
          enable = true;
          autoLogin = mkIf (cfg.autoLoginUser != null) {
            enable = true;
            user = cfg.autoLoginUser;
          };
        };

        layout = "us";
        windowManager = {
          default = "xinitrc";
          session = [{
            name = "xinitrc";
            start = ''
              ${xinitrc} &
              waitPID=$!
            '';
          }];
        };
      };

      redshift = {
        enable = true;
        provider = "geoclue2";
      };
    };
  };
}
