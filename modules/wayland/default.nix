{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles.desktop;

  alacrittyWrapped = pkgs.callPackage ../../pkgs/alacritty-config {
    config.fontSize = cfg.alacritty.font.size;
  };

  wayland-configs = pkgs.runCommand "wayland-configs" {} ''
    install -Dm644 ${./hyprland.conf} $out/etc/xdg/configctl/hyprland.conf
    install -Dm644 ${./waybar-config} $out/etc/xdg/configctl/waybar-config
    install -Dm644 ${./waybar-style.css} $out/etc/xdg/configctl/waybar-style.css
    install -Dm644 ${./wofi.css} $out/etc/xdg/configctl/wofi.css
    install -Dm644 ${./wofi.conf} $out/etc/xdg/configctl/wofi.conf
  '';

  swayPkg = config.programs.sway.package;
  start-session = pkgs.writeShellScript "start-start-session" ''
    ${pkgs.rustybox}/bin/configctl init

    # exec ${config.programs.hyprland.package}/bin/Hyprland --config $XDG_RUNTIME_DIR/configctl/hyprland.conf
    ${swayPkg}/bin/${swayPkg.meta.mainProgram}
  '';

  lock-now = pkgs.writeShellApplication {
    name = "lock-now";
    runtimeInputs = [pkgs.swaylock-effects];
    text = "exec swaylock -f -C ${./swaylock.conf}";
  };

  wofi-themed = pkgs.symlinkJoin {
    name = "wofi-themed";
    paths = [pkgs.wofi];
    buildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram $out/bin/wofi \
        --add-flags '--style=$XDG_RUNTIME_DIR/configctl/wofi.css' \
        --add-flags '--conf=$XDG_RUNTIME_DIR/configctl/wofi.conf' \
        --add-flags '--gtk-dark'
    '';
  };

  wofi-windows = pkgs.writeShellApplication {
    name = "wofi-windows";
    runtimeInputs = [pkgs.hyprland pkgs.jq wofi-themed];
    text = ''
      hyprctl dispatch focuswindow title:"$(hyprctl clients -j | jq -r '.[] | select(.mapped).title' | wofi --show dmenu --prompt window --insensitive)"
    '';
  };
in {
  options.dotfiles.desktop = {
    alacritty.font.size = lib.mkOption {
      default = "10.0";
      description = "Font size of Alacritty terminal windows.";
      type = lib.types.str;
    };

    autoLoginUser = lib.mkOption {
      default = null;
      description = "Automatically login as this user on startup.";
      type = lib.types.nullOr lib.types.str;
    };

    enableNvidia = lib.mkOption {
      default = false;
      description = "Whether to enable NVIDIA graphics drivers.";
      type = lib.types.bool;
    };
  };

  config = {
    environment.systemPackages = [
      alacrittyWrapped
      lock-now
      wayland-configs
      wofi-themed
      wofi-windows
      pkgs.alacritty
      pkgs.dunst
      pkgs.grim
      pkgs.hyprland
      pkgs.hyprland-protocols
      pkgs.hyprpaper
      pkgs.lxappearance
      pkgs.polkit-kde-agent
      pkgs.slurp
      pkgs.swayidle
      pkgs.swaylock-effects
      pkgs.waybar
      pkgs.wev
      pkgs.wl-clipboard
      pkgs.wlogout
      pkgs.wlogout
      pkgs.wlopm
      pkgs.wlr-randr
    ];

    boot.extraModprobeConfig = lib.mkIf cfg.enableNvidia ''
      options nvidia_drm modeset=1 fbdev=1
    '';

    security.pam.services.swaylock = {};

    environment.sessionVariables =
      {
        # XCURSOR_THEME = "Adwaita";
        XCURSOR_SIZE = "40";
        MOZ_ENABLE_WAYLAND = "1";
        NIXOS_OZONE_WL = "1";
        QT_AUTO_SCREEN_SCALE_FACTOR = "1";
        QT_QPA_PLATFORM = "wayland;xcb";
        QT_QPA_PLATFORMTHEME = "qt5ct";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        GDK_BACKEND = "wayland,x11";
        SDL_VIDEO_DRIVER = "wayland";
        CLUTTER_BACKEND = "wayland";
        # GTK_THEME = "Nord";

        XDG_CACHE_HOME = "$HOME/.local/cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_DESKTOP_DIR = "$HOME/desktop";
        XDG_DOCUMENTS_DIR = "$HOME/documents";
        XDG_DOWNLOAD_DIR = "$HOME/downloads";
        XDG_MUSIC_DIR = "$HOME/media/music";
        XDG_PICTURES_DIR = "$HOME/media/img";
        XDG_PUBLICSHARE_DIR = "$HOME/.share";
        XDG_STATE_HOME = "$HOME/.local/state";
        XDG_TEMPLATES_DIR = "$HOME/.templates";
        XDG_VIDEOS_DIR = "$HOME/media/videos";
      }
      // lib.attrsets.optionalAttrs cfg.enableNvidia {
        LIBVA_DRIVER_NAME = "nvidia";
        WLR_NO_HARDWARE_CURSORS = "1";
        GBM_BACKEND = "nvidia-drm";
        NVD_BACKEND = "direct";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
        ELECTRON_OZONE_PLATFORM_HINT = "auto";
      };

    programs = {
      hyprland = {
        enable = true;
        xwayland.enable = true;
      };

      sway = {
        enable = true;
        package = pkgs.swayfx;
        wrapperFeatures.gtk = true;
      };
    };

    systemd.user.services = {
      gammastep = {
        inherit (pkgs.waybar.meta) description;
        wantedBy = ["sway-session.target"];
        serviceConfig.ExecStart = "${pkgs.gammastep}/bin/gammastep";
      };

      waybar = {
        inherit (pkgs.waybar.meta) description;
        wantedBy = ["sway-session.target"];
        serviceConfig.ExecStart = pkgs.writeShellScript "start-waybar" ''
          exec ${pkgs.waybar}/bin/waybar \
            --config $XDG_RUNTIME_DIR/configctl/waybar-config \
            --style $XDG_RUNTIME_DIR/configctl/waybar-style.css
        '';
      };

      hyprpaper = {
        inherit (pkgs.hyprpaper.meta) description;
        wantedBy = ["hyprland.target"];
        serviceConfig.ExecStart = "${pkgs.hyprpaper}/bin/hyprpaper";
      };

      swayidle = {
        inherit (pkgs.swayidle.meta) description;
        # TODO: replace me with hyperland.target
        wantedBy = ["sway-session.target"];
        serviceConfig.ExecStart = "${pkgs.swayidle}/bin/swayidle -w -C ${./swayidle.conf}";
        path = [lock-now config.programs.hyprland.package];
      };
    };

    # https://github.com/danth/stylix

    systemd.user.targets.hyprland = {
      unitConfig = {
        Description = "Hyprland target.";
        BindsTo = "graphical-session.target";
      };
    };

    services.greetd = {
      enable = true;
      settings = {
        vt = 1;
        default_session = {
          command = ''
            ${start-session}
            /run/current-system/systemd/bin/systemctl --user stop graphical-session.target
          '';
          user = cfg.autoLoginUser;
        };
      };
    };

    services.geoclue2.enable = true;

    services.keyd = {
      enable = true;

      keyboards = {
        default = {
          ids = ["*"];
          settings = {
            main = {
              capslock = "overload(control, esc)";
              # TODO: if statement here
              leftmeta = "layer(alt)";
              leftalt = "layer(meta)";
              rightalt = "layer(meta)";
              rightcontrol = "layer(alt)";
            };
          };
        };
      };
    };

    xdg.portal = {
      enable = true;
      extraPortals =
        [
          pkgs.xdg-desktop-portal
          pkgs.xdg-desktop-portal-wlr
          pkgs.xdg-desktop-portal-gtk
        ]
        # the nvidia patches automatically enable the hyprland desktop portal
        # ++ lib.optional (!cfg.enableNvidia) pkgs.xdg-desktop-portal-hyprland
        ;
    };
  };
}
