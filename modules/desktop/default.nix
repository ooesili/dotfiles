{ config, pkgs, lib, ... }:

let
  cfg = config.dotfiles.desktop;

  alacrittyWrapped = pkgs.callPackage ../../pkgs/alacritty-config {
    config.fontSize = cfg.alacritty.font.size;
  };

  lockScript = pkgs.writeShellApplication {
    name = "lock-script";
    runtimeInputs = [
      pkgs.i3lock
      pkgs.xorg.xset
    ];

    text = ''
      i3lock --color=000000
      xset dpms force off
    '';
  };

  xmodmaprc = pkgs.writeText "xmodmaprc" ''
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

  xinitrc = pkgs.writeShellApplication {
    name = "xinitrc";
    runtimeInputs = [
      pkgs.feh
      pkgs.rustybox
      pkgs.xorg.xmodmap
      pkgs.xorg.xset
      pkgs.xorg.xsetroot
    ];

    text = ''
      feh --no-fehbg --bg-fill "$(cat ~/.wallpaper)"
      xsetroot -cursor_name left_ptr

      ${if cfg.xmodmap.enable then ''
      # custom keybindings
      # xmodmap exits 1 always for whatever reason
      xmodmap ${xmodmaprc} || true
      '' else ""}

      # set keyboard repeat rate
      delay=200 # ms
      rate=30 # Hz
      xset r rate "$delay" "$rate"
    '';
  };

  i3-config = pkgs.runCommand "i3-config" {
    alacritty = "${alacrittyWrapped}/bin/alacritty";
    amixer = "${pkgs.alsaUtils}/bin/amixer";
    audioMode = "${pkgs.audio-mode}/bin/audio-mode";
    brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
    i3 = pkgs.i3-gaps;
    i3status = "${pkgs.i3status}/bin/i3status";
    lockScript = "${lockScript}/bin/lock-script";
    pamixer = "${pkgs.pamixer}/bin/pamixer";
    playerctl = "${pkgs.playerctl}/bin/playerctl";
    rofi = "${pkgs.rofi}/bin/rofi";
  } ''
    mkdir -p $out/etc/xdg/configctl
    substituteAll ${./i3-config} $out/etc/xdg/configctl/i3-config
  '';

in with lib; {
  options.dotfiles.desktop = {
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

  config = {
    environment.systemPackages = with pkgs; [
      alacritty
      alacrittyWrapped
      flameshot
      i3-config
      i3-gaps
      libnotify
      lockScript
      rofi
      xorg.xev
      xsel
    ];

    services.redshift.enable = true;

    systemd.user.services = {
      unclutter = {
        description = "Hides the X11 cursor on inactivity.";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig.ExecStart = "${pkgs.unclutter-xfixes}/bin/unclutter --jitter 5";
      };

      xautolock = {
        description = "Locks the screen after a period of inactivity";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig.ExecStart = "${pkgs.xautolock}/bin/xautolock -time 15 -locker ${lockScript}/bin/lock-script";
      };

      xcape = {
        description = "Tap the control key to send escape.";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape -f";
        after = [ "xinit.service" ];
      };

      xinit = {
        description = "X11 one-time startup commands.";
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          ExecStart = "${xinitrc}/bin/xinitrc";
          Type = "oneshot";
        };
      };
    };

    services.xserver = {
      enable = true;

      displayManager = {
        defaultSession = "none+i3";
        lightdm.enable = true;

        autoLogin = mkIf (cfg.autoLoginUser != null) {
          enable = true;
          user = cfg.autoLoginUser;
        };
      };

      windowManager.session = [{
        name  = "i3";
        bgSupport = true;
        start = ''
          # environment variables
          # shellcheck source=/dev/null
          . ${cfg.shellProfile}

          # create live-editing symlink for i3
          ${pkgs.rustybox}/bin/configctl init

          ${pkgs.i3-gaps}/bin/i3 -c $XDG_RUNTIME_DIR/configctl/i3-config &
          waitPID=$!
        '';
      }];

      layout = "us";
    };
  };
}
