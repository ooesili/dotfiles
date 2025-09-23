{
  config,
  pkgs,
  ...
}: {
  imports = [
    ../workstation.nix
    ./hardware-configuration.nix
    ../../modules/obs-studio.nix
  ];

  boot = {
    blacklistedKernelModules = ["snd_hda_intel"];
    kernelParams = ["nvidia-drm.modeset=1"];
    kernelModules = [
      "af_key"
      "nvidia"
      "nvidia_modeset"
      "nvidia_uvm"
      "nvidia_drm"
    ];
  };

  dotfiles = {
    desktop = {
      alacritty.font.size = "9.0";
      autoLoginUser = config.dotfiles.primaryUser;
      enableNvidia = true;
    };

    mpd = {
      enable = true;
      user = config.dotfiles.primaryUser;
      dataDir = "/home/${config.dotfiles.primaryUser}/.local/share/mpd";
      musicDir = "/media/exthd-a/files/music/sorted";
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

  networking = {
    hostName = "nixbox";
    firewall = {
      allowedTCPPorts = [
        22000 # syncthing
        55000 # nix-serve
      ];
    };
  };

  hardware = {
    graphics = {
      enable = true;
      extraPackages = [
        pkgs.nvidia-vaapi-driver
      ];
    };

    nvidia = {
      open = false;
      modesetting.enable = true;
      forceFullCompositionPipeline = true;
      package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    };
  };

  services.mpd.settings = {
    audio_output = [
      {
        type = "httpd";
        name = "HTTP Stream";
        encoder = "lame";
        bind_to_address = "[::]";
        port = "6680";
        bitrate = "320"; # do not define if quality is defined
        format = "44100:16:1";
        always_on = "yes"; # prevent MPD from disconnecting all listeners when playback is stopped.
        tags = "yes"; # httpd supports sending tags to listening streams.
      }
    ];
  };

  services.nix-serve = {
    enable = true;
    port = 55000;
    secretKeyFile = "/etc/secrets/nix-serve-key";
  };

  services.restic.backups.main = {
    # check out dynamicBackupsFrom
    passwordFile = "/etc/secrets/restic-backup-key";
    paths = [
      "/etc/secrets"
      "/home/ooesili/.aws"
      "/home/ooesili/.config/Element"
      "/home/ooesili/.config/SuperCollider"
      "/home/ooesili/.config/clojure"
      "/home/ooesili/.config/direnv/direnvrc"
      "/home/ooesili/.config/htop/htoprc"
      "/home/ooesili/.config/patchagerc"
      "/home/ooesili/.config/rncbc.org/QjackCtl.conf"
      "/home/ooesili/.config/syncthing"
      "/home/ooesili/.gitconfig"
      "/home/ooesili/.gitignore_global"
      "/home/ooesili/.gnupg"
      "/home/ooesili/.hydrogen"
      "/home/ooesili/.jackdrc"
      "/home/ooesili/.lein"
      "/home/ooesili/.local/share/TelegramDesktop"
      "/home/ooesili/.local/share/mpd"
      "/home/ooesili/.local/share/nvim"
      "/home/ooesili/.mozilla"
      "/home/ooesili/.nixpkgs/config.nix"
      "/home/ooesili/.ssh"
      "/home/ooesili/.wallpaper"
      "/home/ooesili/.z"
      "/home/ooesili/.zsh_history"
      "/home/ooesili/archive"
      "/home/ooesili/bin"
      "/home/ooesili/books"
      "/home/ooesili/docs"
      "/home/ooesili/images"
      "/home/ooesili/obsidian-notes"
      "/home/ooesili/src"
      "/home/ooesili/studio"
      "/home/ooesili/sync"
      "/home/ooesili/videos"
    ];
    pruneOpts = [
      "--keep-daily 30"
      "--keep-monthly 12"
    ];
    repository = "/media/exthd-a/files/restic-backup";
    timerConfig = {OnCalendar = "daily";};
    user = "root";
  };

  # A confusing namee, but this defines drivers for wayland as well
  services.xserver.videoDrivers = ["nvidia"];

  time.timeZone = "America/Denver";
  virtualisation.virtualbox.host.enable = true;

  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
