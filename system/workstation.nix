{ config, lib, pkgs, unstable, options, ... }:

let
  cfg = config.dotfiles;
  discord-fork = import ../pkgs/discord {
    branch = "stable";
    inherit pkgs;
  };
  leiningenJDK11 = pkgs.leiningen.override { jdk = pkgs.openjdk11; };
  tmuxConfig = pkgs.callPackage ../pkgs/tmux-config {
    copyCommand = "${pkgs.rustybox}/bin/rclip copy --clipboard";
    pasteCommand = "${pkgs.rustybox}/bin/rclip paste --clipboard";
  };
  zshConfig = pkgs.callPackage ../pkgs/zsh-config {};

in with lib; {
  imports = [
    ../modules/cloudflare-ddns
    ../modules/desktop
    ../modules/dunst.nix
    ../modules/mpd
    ../modules/pro-audio
    ../modules/quil.nix
    ../modules/tailscale.nix
    ../modules/trusts.nix
    ../modules/yubikey.nix
  ];

  options = {
    dotfiles = {
      primaryUser = mkOption {
        description = "Primary admin user of the system.";
        example = "rshackleford";
        type = types.str;
      };

      soundCard = mkOption {
        description = "Primary sound card name (from /proc/asound/cards).";
        example = "PCH";
        type = types.str;
      };
    };
  };

  config = {
    boot = {
      # Use QEMU to build packages for the Raspberry PI
      binfmt.emulatedSystems = [ "aarch64-linux" ];

      loader.efi.canTouchEfiVariables = true;
      loader.systemd-boot.enable = true;

      # Disables this warning:
      # warning: Enabling both boot.enableContainers & virtualisation.containers on system.stateVersion < 22.05 is unsupported.
      enableContainers = false;
    };

    console = {
      # base16 dark colors
      colors = [
        "181818" "ab4642" "a1b56c" "f7ca88"
        "7cafc2" "ba8baf" "86c1b9" "d8d8d8"
        "585858" "181818" "ab4642" "a1b56c"
        "f7ca88" "7cafc2" "ba8baf" "f8f8f8"
      ];

      font = "Lat2-Terminus16";
      keyMap = pkgs.keymap-us-capsctrl;
    };

    dotfiles.desktop.shellProfile = "${zshConfig}/etc/zprofile";

    environment = {
      etc."zprofile.local".text = ". ${zshConfig}/etc/zprofile";
      etc."zshrc.local".text = ". ${zshConfig}/etc/zshrc";

      systemPackages = with pkgs; let
        pythonPackages = py-pkgs: with py-pkgs; [ virtualenv ];
        python = python3.withPackages pythonPackages;
      in [
        age
        asciinema
        awscli
        binutils
        caddy
        coreutils
        direnv
        discord-fork
        dnsutils
        element-desktop
        exa
        fd
        feh
        fennel
        ffmpeg
        file
        firefox
        fzf
        gcc
        gdb
        gimp
        git
        gnumake
        gnupg
        gptfdisk
        htop
        httpie
        hunspellDicts.en-us
        imagemagick
        jq
        leiningenJDK11
        libbpf
        libreoffice
        lorri
        lsof
        luajit_2_1
        luajit_2_1.pkgs.luacheck
        man-pages
        mpv
        mupdf
        ncdu
        neofetch
        neovim
        nmap
        nodePackages.bash-language-server
        nodejs-16_x
        nopt
        p7zip
        pamixer
        paprefs
        pavucontrol
        pinentry-gtk2
        playerctl
        posix_man_pages
        procs
        pv
        python
        ranger
        restic
        ripgrep
        rtorrent
        rustup
        rustybox
        scrot
        shellcheck
        slack
        socat
        sops
        tcpdump
        tmate
        tmux
        tmuxConfig
        tokei
        unixtools.xxd
        unstable.go_1_19
        unstable.godoc
        unstable.gopls
        unstable.inkscape
        unstable.nodePackages.typescript-language-server
        unstable.rust-analyzer
        unstable.spotify
        unstable.tdesktop
        unstable.zig
        usbutils
        viddy
        watchexec
        wine
        wireguard-tools
        zdirs
        zls
      ];
    };

    fonts.fonts = [
      pkgs.hack-font
      pkgs.nerdfonts
      pkgs.siji
      pkgs.unifont
    ];

    i18n.defaultLocale = "en_US.UTF-8";

    networking = {
      firewall = {
        # warning: Strict reverse path filtering breaks Tailscale exit node use
        # and some subnet routing setups. Consider setting
        # `networking.firewall.checkReversePath` = 'loose'
        checkReversePath = "loose";
      };

      dhcpcd.enable = false;
      networkmanager.enable = true;
    };

    nix = {
      package = unstable.nix;
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
    programs.zsh.enable = true;

    security.sudo.extraRules = [
      {
        commands = [ {
          command = "ALL";
          options = [ "SETENV" "NOPASSWD" ];
        } ];
        groups = [ "wheel" ];
        host = "ALL";
        runAs = "ALL:ALL";
        users = [ ];
      }
    ];

    services.fstrim.enable = true;
    services.lorri.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
      jack.enable = true;
    };

    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_14;
    };

    services.tailscale.enable = true;

    sound = {
      enable = true;
      extraConfig = "defaults.pcm.!card ${cfg.soundCard}";
    };

    systemd.user.services.syncthing = {
      description = "Syncthing - Open Source Continuous File Synchronization";
      documentation = [ "man:syncthing(1)" ];
      startLimitIntervalSec = 60;
      startLimitBurst = 4;
      wantedBy = [ "default.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.syncthing}/bin/syncthing serve --no-browser --no-restart --logflags=0";
        Restart = "on-failure";
        RestartSec = 1;
        SuccessExitStatus = [ 3 4 ];
        RestartForceExitStatus = [ 3 4 ];

        # Hardening
        SystemCallArchitectures = [ "native" ];
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
      };
    };

    systemd.user.services.playerctld = {
      description = "Playerctl daemon to keep track of the active media player.";
      documentation = [ "man:playerctld(1)" ];
      startLimitIntervalSec = 60;
      startLimitBurst = 4;
      wantedBy = [ "default.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.playerctl}/bin/playerctld";
        Restart = "on-failure";
        RestartSec = 1;
      };
    };

    systemd.user.services.rclipd = {
      description = "Network copy backend for tmux based on xsel.";
      wantedBy = [ "graphical-session.target" ];
      serviceConfig.ExecStart = "${pkgs.rustybox}/libexec/rclipd";
    };

    users.users."${cfg.primaryUser}" = {
      extraGroups = [
        "adbusers"
        "audio"
        "networkmanager"
        "podman"
        "vboxusers"
        "video"
        "wheel"
      ];
      isNormalUser = true;
      shell = pkgs.zsh;
      uid = 1000;
    };

    virtualisation.podman = {
      enable = true;
      dockerSocket.enable = true;
      defaultNetwork.dnsname.enable = true;
    };
  };
}
