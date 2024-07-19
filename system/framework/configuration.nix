{pkgs, ...}: {
  imports = [
    ../workstation.nix
    ./hardware-configuration.nix
    ../../modules/mac-changer.nix
  ];

  boot.kernelParams = ["mem_sleep_default=deep"];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  hardware.cpu.intel.updateMicrocode = true;

  hardware.graphics.extraPackages = [
    pkgs.libvdpau-va-gl # vdpau bridge for vaapi drivers
    pkgs.vaapiIntel #
    pkgs.vpl-gpu-rt
  ];

  environment.systemPackages = [
    pkgs.brightnessctl
  ];

  environment.variables = {
    VDPAU_DRIVER = "va_gl";
    MOZ_USE_OMTC = "1";
  };

  sec.macchanger = {
    enable = true;
    devices = ["wlp170s0"];
  };

  networking.hostName = "framework";

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
    powertop.enable = true;
  };

  services.upower.enable = true;

  services.tlp = {
    enable = true;
    settings = {
      # cpu scaling
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      # battery charging
      #START_CHARGE_THRESH_BAT1 =  60;
      #STOP_CHARGE_THRESH_BAT1 =  0;
      DEVICES_TO_DISABLE_ON_LAN_CONNECT = "wifi wwan";
      DEVICES_TO_DISABLE_ON_WIFI_CONNECT = "wwan";
      DEVICES_TO_DISABLE_ON_WWAN_CONNECT = "wifi";
    };
  };

  services.libinput = {
    enable = true;
    touchpad.disableWhileTyping = true;
  };

  systemd.user.services.batteryd = {
    description = "A daemon for battery status notifications.";
    wantedBy = ["graphical-session.target"];

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
