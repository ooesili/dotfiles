{ config, lib, pkgs, unstable, ... }:

{
  imports = [
    ../workstation.nix
    ./hardware-configuration.nix
    ../../modules/obs-studio.nix
  ];

  boot = {
    blacklistedKernelModules = [ "snd_hda_intel" ];
    extraModulePackages = [
      # Currently broken: https://github.com/NixOS/nixpkgs/pull/174109
      # config.boot.kernelPackages.rtl88x2bu
    ];
    kernelModules = [ "af_key" ];
  };

  dotfiles = {
    cloudflareDDNS.enable = true;

    desktop = {
      alacritty.font.size = "9.0";
      autoLoginUser = config.dotfiles.primaryUser;
      xmodmap.enable = false;
    };

    mpd = {
      enable = true;
      user = config.dotfiles.primaryUser;
    };

    primaryUser = "ooesili";
    soundCard = "Multibit";
  };

  environment.systemPackages = [
    pkgs.audio-mode
    pkgs.easytag
    pkgs.mdloader
    unstable.blender
  ];

  hardware.opengl.enable = true;
  networking.hostName = "nixbox";
  programs.adb.enable = true;
  programs.steam.enable = true;

  services.dockerRegistry = {
    enable = true;
    enableGarbageCollect = true;
    listenAddress = "0.0.0.0";
  };

  services.mpd.extraConfig = ''
    audio_output {
      type            "httpd"
      name            "HTTP Stream"
      encoder         "lame"
      bind_to_address "[::]"
      port            "6680"
      bitrate         "320"      # do not define if quality is defined
      format          "44100:16:1"
      always_on       "yes"      # prevent MPD from disconnecting all listeners when playback is stopped.
      tags            "yes"      # httpd supports sending tags to listening streams.
    }
  '';

  services.xserver = {
    dpi = 96;
    videoDrivers = [ "nvidia" ];
    screenSection = ''
      Option "metamodes" "HDMI-0: nvidia-auto-select +1920+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}, DP-0: nvidia-auto-select +0+0 {ForceCompositionPipeline=On, ForceFullCompositionPipeline=On}"
    '';
  };

  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
