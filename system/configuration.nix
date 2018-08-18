{ config, pkgs, lib, ... }:

let
  primaryUser = "ooesili";

in with lib; {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.extraModprobeConfig = ''
    blacklist snd_hda_intel
  '';

  networking.hostName = "nixbox";
  time.timeZone = "America/Chicago";

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    alsaUtils
    bind
    binutils
    gcc
    git
    gnumake
    gptfdisk
    neovim
    nmap
    pciutils
    tcpdump
    usbutils
    unzip
  ];

  fonts.fonts = with pkgs; [
    hack-font
  ];

  programs.zsh.enable = true;

  environment.etc."zprofile.local".text = ''
    if test -f ~/.nix-profile/etc/zprofile; then
      . ~/.nix-profile/etc/zprofile
    fi
  '';

  environment.etc."zshrc.local".text = ''
    if test -f ~/.nix-profile/etc/zshrc; then
      . ~/.nix-profile/etc/zshrc
    fi
  '';

  services.openssh.enable = true;

  sound.enable = true;
  sound.extraConfig = ''
    defaults.pcm.!card Multibit
  '';

  services.mpd = {
    enable = true;
    musicDirectory = "/home/${primaryUser}/exthd/files/music";
    dataDir = "/home/${primaryUser}/.local/share/mpd";
    user = primaryUser;
    group = "users";

    extraConfig = ''
      audio_output {
        type       "alsa"
        name       "Modi Multibit"
        device     "default:CARD=Multibit"
        mixer_type "none"
      }
    '';
  };

  security.hideProcessInformation = true;

  networking.firewall.allowedTCPPorts = [
    22000 # syncthing
    31337 # netcat
  ];
  networking.firewall.allowedUDPPorts = [
    21027 # syncthing local discovery
  ];

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.displayManager.lightdm = {
    enable = true;
    autoLogin = {
      enable = true;
      user = primaryUser;
    };
  };

  services.xserver.desktopManager.default = "none";
  services.xserver.windowManager.default = "xinitrc";
  services.xserver.windowManager.session = [{
    name = "xinitrc";
    start = ''
      "$HOME/.nix-profile/etc/xinitrc" &
      waitPID=$!
    '';
  }];

  users.users."${primaryUser}" = {
    extraGroups = [ "audio" "wheel" ];
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };

  users.users.guest = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1001;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.headless = true;
}
