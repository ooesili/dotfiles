{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../workstation.nix
    ./hardware-configuration.nix
    ../../modules/obs-studio.nix
  ];

  boot = {
    blacklistedKernelModules = ["snd_hda_intel"];
    kernelModules = ["af_key"];
  };

  dotfiles = {
    desktop = {
      alacritty.font.size = "9.0";
      autoLoginUser = config.dotfiles.primaryUser;
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
    pkgs.blender
  ];

  hardware.cpu.intel.updateMicrocode = true;
  hardware.graphics.enable = true;
  networking.hostName = "nixbox";
  programs.adb.enable = true;
  programs.steam.enable = true;

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

  virtualisation.virtualbox.host.enable = true;

  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
