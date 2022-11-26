{ config, lib, pkgs, ... }:

{
  imports = [
    ../workstation.nix
    ./hardware-configuration.nix
    ../../modules/mac-changer.nix
  ];

  boot.kernelParams = [ "mem_sleep_default=deep" ];
  boot.kernelPackages = pkgs.linuxPackages_latest;

  sec.macchanger = {
    enable = true;
    devices = [ "wlp170s0" ];
  };

  networking.hostName = "framework";

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
    powertop.enable = true;
  };

  services.fprintd.enable = true;
  services.xserver = {
    dpi = 136;

    libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
    };
  };

  systemd.user.services.batteryd = {
    description = "A daemon for battery status notifications.";
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.rustybox}/libexec/batteryd";
      ProtectSystem = "strict";
      Restart = "always";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
