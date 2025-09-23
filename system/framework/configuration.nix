{pkgs, ...}: {
  imports = [
    ../workstation.nix
    ./hardware-configuration.nix
    ../../modules/mac-changer.nix
  ];

  dotfiles = let
    primaryUser = "ooesili";
  in {
    desktop = {
      alacritty.font.size = "9.5";
      autoLoginUser = primaryUser;
    };

    inherit primaryUser;
    soundCard = "PCH";
  };

  boot.kernelParams = ["mem_sleep_default=deep"];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  hardware.cpu.intel.updateMicrocode = true;

  hardware.graphics.extraPackages = [
    pkgs.libvdpau-va-gl # vdpau bridge for vaapi drivers
    pkgs.intel-vaapi-driver
    pkgs.vpl-gpu-rt
  ];

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  boot.extraModprobeConfig = ''
    # HDMI audio output doesn't work without this
    # https://community.frame.work/t/resolved-no-audio-via-hdmi/39823/11
    options snd-intel-dspcfg dsp_driver=1
  '';

  environment.systemPackages = [
    pkgs.pkg-config
    pkgs.openssl
    pkgs.brightnessctl
    pkgs.yt-dlp
  ];

  environment.variables = {
    VDPAU_DRIVER = "va_gl";
    MOZ_USE_OMTC = "1";
  };

  sec.macchanger = {
    enable = false;
    devices = ["wlp170s0"];
  };
  networking = {
    hostName = "framework";
    networkmanager.wifi.powersave = false;
  };

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
      CPU_BOOST_ON_AC = 1;
      CPU_BOOST_ON_BAT = 0;
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

  # TODO: disable touchpad while typing

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
