{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles;
in {
  imports = [
    ../modules/cloudflare-ddns
    ../modules/dunst.nix
    ../modules/wayland
    ../modules/virtualbox.nix
    ../modules/mpd
    ../modules/printing.nix
    ../modules/ghostty
    ../modules/pro-audio
    ../modules/shell
    ../modules/trusts.nix
    ../modules/yubikey.nix
  ];

  options = {
    dotfiles = {
      primaryUser = lib.mkOption {
        description = "Primary admin user of the system.";
        example = "rshackleford";
        type = lib.types.str;
      };

      soundCard = lib.mkOption {
        description = "Primary sound card name (from /proc/asound/cards).";
        example = "PCH";
        type = lib.types.str;
      };
    };
  };

  config = {
    boot = {
      # Use QEMU to build ackages for the Raspberry PI
      binfmt.emulatedSystems = ["aarch64-linux"];

      loader.efi.canTouchEfiVariables = true;
      loader.systemd-boot.enable = true;
      loader.systemd-boot.configurationLimit = 25;

      # Disables this warning:
      # warning: Enabling both boot.enableContainers & virtualisation.containers on system.stateVersion < 22.05 is unsupported.
      enableContainers = false;

      tmp.useTmpfs = true;
    };

    console = {
      # base16 dark colors
      colors = [
        "181818"
        "ab4642"
        "a1b56c"
        "f7ca88"
        "7cafc2"
        "ba8baf"
        "86c1b9"
        "d8d8d8"
        "585858"
        "181818"
        "ab4642"
        "a1b56c"
        "f7ca88"
        "7cafc2"
        "ba8baf"
        "f8f8f8"
      ];

      font = "Lat2-Terminus16";
      keyMap = pkgs.keymap-us-capsctrl;
    };

    documentation.man.cache = {
      enable = true;
      generateAtRuntime = true;
    };

    environment = {
      sessionVariables = {
        BROWSER = "firefox";
      };

      systemPackages = let
        pythonPackages = py-pkgs: [py-pkgs.virtualenv];
        python = pkgs.python3.withPackages pythonPackages;
      in [
        python

        pkgs.age
        pkgs.alejandra
        pkgs.asciinema
        pkgs.awscli2
        pkgs.bacon
        pkgs.bash-language-server
        pkgs.bat
        pkgs.binutils
        pkgs.bottom
        pkgs.caddy
        pkgs.cargo-nextest
        pkgs.chromium
        pkgs.clang-tools
        pkgs.claude-code
        pkgs.colima
        pkgs.coppwr
        pkgs.coreutils
        pkgs.deadnix
        pkgs.delta
        pkgs.discord
        pkgs.dnsutils
        pkgs.dyff
        pkgs.eza
        pkgs.fd
        pkgs.ffmpeg
        pkgs.file
        pkgs.firefox
        pkgs.gcc
        pkgs.gdb
        pkgs.gimp
        pkgs.git
        pkgs.gnumake
        pkgs.gnupg
        pkgs.go_1_27
        pkgs.gopls
        pkgs.gotools
        pkgs.gptfdisk
        pkgs.httpie
        pkgs.hunspellDicts.en-us
        pkgs.imagemagick
        pkgs.imv
        pkgs.inkscape
        pkgs.ipcalc
        pkgs.jless
        pkgs.jq
        pkgs.kubectl
        pkgs.lazygit
        pkgs.lua-language-server
        pkgs.leiningenJDK11
        pkgs.libreoffice
        pkgs.lls
        pkgs.lsof
        pkgs.luajit_2_1
        pkgs.luajit_2_1.pkgs.luacheck
        pkgs.man-pages
        pkgs.man-pages-posix
        pkgs.mprocs
        pkgs.mpv
        pkgs.mupdf
        pkgs.ncdu
        pkgs.neovim
        pkgs.nil
        pkgs.nmap
        pkgs.nodejs_26
        pkgs.obsidian
        pkgs.opencode
        pkgs.p7zip
        pkgs.pamixer
        pkgs.pavucontrol
        pkgs.pciutils
        pkgs.pgcli
        pkgs.pinentry-gnome3
        pkgs.playerctl
        pkgs.procs
        pkgs.pv
        pkgs.pyright
        pkgs.restic
        pkgs.ripgrep
        pkgs.rlwrap
        pkgs.rust-analyzer
        pkgs.rust-bin.stable.latest.default
        pkgs.rustybox
        pkgs.shellcheck
        pkgs.socat
        pkgs.sops
        pkgs.spotify
        pkgs.statix
        pkgs.tcpdump
        pkgs.tokei
        pkgs.typescript-language-server
        pkgs.unixtools.xxd
        pkgs.usbutils
        pkgs.viddy
        pkgs.watchexec
        pkgs.wireguard-tools
        pkgs.yazi
        pkgs.zls
        pkgs.zoom-us
      ];
    };

    fonts.packages = [
      pkgs.nerd-fonts.hack
      pkgs.nerd-fonts.noto
      pkgs.siji
      pkgs.unifont
    ];

    hardware.keyboard.qmk.enable = true;

    i18n.defaultLocale = "en_US.UTF-8";

    location = {
      provider = "manual";
      # https://location.services.mozilla.com/v1/geolocate?key=geoclue

      # Denver
      latitude = 39.6888;
      longitude = -105.156;
    };

    networking = {
      firewall = {
        # warning: Strict reverse path filtering breaks Tailscale exit node use
        # and some subnet routing setups. Consider setting
        # `networking.firewall.checkReversePath` = 'loose'
        checkReversePath = "loose";

        trustedInterfaces = ["tailscale0"];

        allowedTCPPorts = [
          31337 # netcat
        ];
        allowedUDPPorts = [
          21027 # syncthing local discovery
          31337 # netcat
        ];
      };

      dhcpcd.enable = false;
      networkmanager.enable = true;
    };

    nix = {
      extraOptions = ''
        experimental-features = nix-command flakes
      '';

      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };
    };

    programs.dunst.enable = true;
    programs.command-not-found.enable = false;

    programs.nix-index = {
      enable = true;
      enableZshIntegration = false;
      enableBashIntegration = false;
      enableFishIntegration = true;
    };

    services.auto-cpufreq.enable = true;
    services.fstrim.enable = true;
    services.thermald.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_19;
    };

    services.tailscale.enable = true;

    services.timesyncd.enable = false;
    services.chrony.enable = true;

    systemd.user.services.syncthing = {
      description = "Syncthing - Open Source Continuous File Synchronization";
      documentation = ["man:syncthing(1)"];
      startLimitIntervalSec = 60;
      startLimitBurst = 4;
      wantedBy = ["default.target"];
      enable = false;

      serviceConfig = {
        ExecStart = "${pkgs.syncthing}/bin/syncthing serve --no-browser --no-restart --logflags=0";
        Restart = "on-failure";
        RestartSec = 1;
        SuccessExitStatus = [
          3
          4
        ];
        RestartForceExitStatus = [
          3
          4
        ];

        # Hardening
        SystemCallArchitectures = ["native"];
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
      };
    };

    systemd.user.services.playerctld = {
      description = "Playerctl daemon to keep track of the active media player.";
      documentation = ["man:playerctld(1)"];
      startLimitIntervalSec = 60;
      startLimitBurst = 4;
      wantedBy = ["default.target"];

      serviceConfig = {
        ExecStart = "${pkgs.playerctl}/bin/playerctld";
        Restart = "on-failure";
        RestartSec = 1;
      };
    };

    systemd.user.services.swayd = {
      description = "Daemon to customize sway using IPC.";
      wantedBy = ["sway-session.target"];
      serviceConfig. ExecStart = "${pkgs.rustybox}/libexec/swayd";
    };

    systemd.user.services.rclipd = {
      description = "Network copy backend for tmux based on xsel.";
      wantedBy = ["sway-session.target"];
      serviceConfig.ExecStart = "${pkgs.rustybox}/libexec/rclipd";
    };

    users.users."${cfg.primaryUser}" = {
      extraGroups = [
        "adbusers"
        "audio"
        "docker"
        "networkmanager"
        "vboxusers"
        "video"
        "wheel"
      ];
      isNormalUser = true;
      shell = pkgs.bash;
      uid = 1000;
    };

    virtualisation.docker.enable = true;
  };
}
