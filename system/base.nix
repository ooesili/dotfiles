{ config, pkgs, lib, ... }:

let
  cfg = config.dotfiles;

  axefx2-firmware = with pkgs; stdenvNoCC.mkDerivation {
    name = "axefx2-firmware";
    src = fetchurl {
      url = https://launchpad.net/~albaguirre/+archive/ubuntu/axe-fx2/+sourcefiles/axefx2-usb-firmware/1.0/axefx2-usb-firmware_1.0.tar.xz;
      sha256 = "0dhawalyh9ah8snymisa1jm1l5n9wjyifp9ws0bhsypl7sv2fhy9";
    };

    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      install -Dm0755 axefx2-usb-fw.hex "$out/share/usb/axefx2.hex"
    '';
  };

in with lib; {
  imports = [
    ./hardware-configuration.nix
    ./local.nix
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
      # jack
      kernelParams = [ "threadirq" ];

      loader.efi.canTouchEfiVariables = true;
      loader.systemd-boot.enable = true;
    };

    environment = {
      etc."zprofile.local".text = ''
        if test -f ~/.nix-profile/etc/zprofile; then
          . ~/.nix-profile/etc/zprofile
        fi
      '';

      etc."zshrc.local".text = ''
        if test -f ~/.nix-profile/etc/zshrc; then
          . ~/.nix-profile/etc/zshrc
        fi
      '';

      systemPackages = with pkgs; [
        alsaUtils
        bind
        binutils
        file
        gcc
        git
        gnumake
        gptfdisk
        lsof
        neovim
        nmap
        pciutils
        tcpdump
        usbutils
        unzip
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

    networking.nameservers = [ "1.1.1.1" ];

    nixpkgs.config.allowUnfree = true;

    programs.zsh.enable = true;

    security = {
      hideProcessInformation = true;

      # jack
      pam.loginLimits = [
        { domain = "@audio"; item = "memlock"; type = "-"   ; value = "unlimited"; }
        { domain = "@audio"; item = "rtprio" ; type = "-"   ; value = "99"       ; }
        { domain = "@audio"; item = "nofile" ; type = "soft"; value = "99999"    ; }
        { domain = "@audio"; item = "nofile" ; type = "hard"; value = "99999"    ; }
      ];
      rtkit.enable = true;
    };

    services = {
      # jack
      das_watchdog.enable = true;

      mpd = {
        dataDir = "/home/${cfg.primaryUser}/.local/share/mpd";
        enable = true;
        group = "users";
        user = cfg.primaryUser;

        extraConfig = if config.hardware.pulseaudio.enable then
          ''
            audio_output {
              type "pulse"
              name "pulse audio"
            }
          ''
        else
          ''
            audio_output {
              type   "alsa"
              name   "Default Output"
              device "default:CARD=${cfg.soundCard}"
            }
          '';
      };

      openssh.enable = true;

      redshift = {
        enable = true;
        latitude = "41.882708";
        longitude = "-87.623306";
      };

      udev.extraRules = ''
        # Fractal Audio Systems Axe-FX II
        ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="2466", ATTR{idProduct}=="0003", RUN+="${pkgs.fxload}/bin/fxload -t fx2 -I ${axefx2-firmware}/share/usb/axefx2.hex -D $env{DEVNAME}"

        # realtime audio
        KERNEL=="rtc0", GROUP="audio"
        KERNEL=="hpet", GROUP="audio"
      '';

      xserver = {
        enable = true;
        desktopManager.default = "none";
        displayManager.lightdm = {
          enable = true;
          autoLogin = {
            enable = true;
            user = cfg.primaryUser;
          };
        };
        layout = "us";
        windowManager = {
          default = "xinitrc";
          session = [{
            name = "xinitrc";
            start = ''
              "$HOME/.nix-profile/etc/xinitrc" &
              waitPID=$!
            '';
          }];
        };
      };
    };

    sound = {
      enable = true;
      extraConfig = ''
        defaults.pcm.!card ${cfg.soundCard}
      '';
    };

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "18.09"; # Did you read the comment?

    time.timeZone = "America/Chicago";

    users.users."${cfg.primaryUser}" = {
      extraGroups = [ "audio" "wheel" ];
      isNormalUser = true;
      shell = pkgs.zsh;
      uid = 1000;
    };
  };
}
