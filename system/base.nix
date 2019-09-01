{ config, lib, pkgs, ... }:

let
  unstable = import <unstable> {};
  cfg = config.dotfiles;
  tmuxConfig = with pkgs; callPackage ./pkgs/tmux-config {
    copyCommand = "${xsel}/bin/xsel -b";
    pasteCommand = "${xsel}/bin/xsel -b";
  };
  neovimConfig = pkgs.callPackage ./pkgs/neovim-config {};
  zshConfig = pkgs.callPackage ./pkgs/zsh-config {};

in with lib; {
  imports = [
    ./hardware-configuration.nix
    ./local.nix
    ./modules/desktop
    ./modules/mpd
    ./modules/pro-audio
    ./modules/quil.nix
    ./modules/yubikey.nix
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
      loader.efi.canTouchEfiVariables = true;
      loader.systemd-boot.enable = true;
    };

    dotfiles.desktop.shellProfile = "${zshConfig}/etc/zprofile";

    environment = {
      etc."zprofile.local".text = ". ${zshConfig}/etc/zprofile";
      etc."zshrc.local".text = ". ${zshConfig}/etc/zshrc";

      systemPackages = with pkgs; let
        pythonPackages = py-pkgs: with py-pkgs; [ virtualenv ];
        python = python3.withPackages pythonPackages;
      in [
        awscli
        bind
        binutils
        capnproto
        cfssl
        coreutils
        direnv
        discord
        easytag
        elmPackages.elm
        elmPackages.elm-format
        exa
        feh
        ffmpeg
        file
        firefox
        fzf
        gcc
        ghc
        gimp
        git
        gnumake
        go_1_11
        gptfdisk
        graphviz
        groff
        htop
        httpie
        hydrogen
        imagemagick
        ipfs
        jq
        keepassx
        leiningen
        lsof
        lua
        man-pages
        mpv
        mupdf
        ncdu
        neovim
        neovimConfig
        nmap
        openjdk8
        p7zip
        pciutils
        posix_man_pages
        pv
        python
        ranger
        reflex
        ripgrep
        ruby_2_5
        rustup
        shellcheck
        socat
        spotify
        sqlite
        stack
        supercollider
        tcpdump
        tdesktop
        tmux
        tmuxConfig
        unixtools.xxd
        unstable.signal-desktop
        unzip
        usbutils
        vagrant
        wireguard
        zip
      ];
    };

    fonts.fonts = with pkgs; [
      hack-font
    ];

    i18n = {
      consoleFont = "Lat2-Terminus16";
      consoleKeyMap = "us";
      defaultLocale = "en_US.UTF-8";
    };

    networking.firewall = {
      allowedTCPPorts = [
        22000 # syncthing
        31337 # netcat
      ];
      allowedUDPPorts = [
        21027 # syncthing local discovery
      ];
    };

    networking.nameservers = [ "1.1.1.1" "1.0.0.1" ];
    nixpkgs.config.allowUnfree = true;
    programs.zsh.enable = true;
    security.hideProcessInformation = true;

    services = {
      openssh.enable = true;
    };

    time.timeZone = "America/Chicago";

    users.users."${cfg.primaryUser}" = {
      extraGroups = [ "audio" "docker" "wheel" ];
      isNormalUser = true;
      shell = pkgs.zsh;
      uid = 1000;
    };

    virtualisation.docker.enable = true;
  };
}
