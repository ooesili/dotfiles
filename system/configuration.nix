{ config, pkgs, lib, ... }:

let
  primaryUser = "ooesili";

in with lib; {
  imports = [
    ./hardware-configuration.nix
  ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    extraModprobeConfig = ''
      blacklist snd_hda_intel
    '';
    kernelParams = [ "threadirq" ];
  };

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
    file
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

  services.das_watchdog.enable = true;
  services.udev.extraRules =
    let axefx2-firmware = pkgs.stdenvNoCC.mkDerivation {
      name = "axefx2-firmware";
      src = pkgs.fetchurl {
        url = https://launchpad.net/~albaguirre/+archive/ubuntu/axe-fx2/+sourcefiles/axefx2-usb-firmware/1.0/axefx2-usb-firmware_1.0.tar.xz;
        sha256 = "0dhawalyh9ah8snymisa1jm1l5n9wjyifp9ws0bhsypl7sv2fhy9";
      };

      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        install -Dm0755 axefx2-usb-fw.hex "$out/share/usb/axefx2.hex"
      '';
    };
    in ''
      # Fractal Audio Systems Axe-FX II
      ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="2466", ATTR{idProduct}=="0003", RUN+="${pkgs.fxload}/bin/fxload -t fx2 -I ${axefx2-firmware}/share/usb/axefx2.hex -D $env{DEVNAME}"

      # realtime audio
      KERNEL=="rtc0", GROUP="audio"
      KERNEL=="hpet", GROUP="audio"
    '';

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

  security.pam.loginLimits = [
    { domain = "@audio"; item = "memlock"; type = "-"   ; value = "unlimited"; }
    { domain = "@audio"; item = "rtprio" ; type = "-"   ; value = "99"       ; }
    { domain = "@audio"; item = "nofile" ; type = "soft"; value = "99999"    ; }
    { domain = "@audio"; item = "nofile" ; type = "hard"; value = "99999"    ; }
  ];

  # to let jackd get realtime permissions
  security.rtkit.enable = true;

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

  services.redshift = {
    enable = true;
    latitude = "41.882708";
    longitude = "-87.623306";
  };

  users.users."${primaryUser}" = {
    extraGroups = [ "audio" "wheel" ];
    isNormalUser = true;
    shell = pkgs.zsh;
    uid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

  virtualisation.virtualbox.host.enable = true;
  nixpkgs.config.virtualbox.enableExtensionPack = true;
}
