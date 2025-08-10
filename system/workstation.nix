{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles;
  leiningenJDK11 = pkgs.leiningen.override {jdk = pkgs.openjdk11;};
in
  with lib; {
    imports = [
      ../modules/cloudflare-ddns
      ../modules/dunst.nix
      ../modules/wayland
      ../modules/mpd
      ../modules/printing.nix
      ../modules/pro-audio
      ../modules/trusts.nix
      ../modules/yubikey.nix
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
        # Use QEMU to build ackages for the Raspberry PI
        binfmt.emulatedSystems = ["aarch64-linux"];

        loader.efi.canTouchEfiVariables = true;
        loader.systemd-boot.enable = true;

        # Disables this warning:
        # warning: Enabling both boot.enableContainers & virtualisation.containers on system.stateVersion < 22.05 is unsupported.
        enableContainers = false;

        tmp.useTmpfs = true;
      };

      console = {
        # base16 dark colors
        colors = [
          "181818"
          "ab4642"
          "a1b56c"
          "f7ca88"
          "7cafc2"
          "ba8baf"
          "86c1b9"
          "d8d8d8"
          "585858"
          "181818"
          "ab4642"
          "a1b56c"
          "f7ca88"
          "7cafc2"
          "ba8baf"
          "f8f8f8"
        ];

        font = "Lat2-Terminus16";
        keyMap = pkgs.keymap-us-capsctrl;
      };

      documentation.man.generateCaches = false;

      environment = {
        sessionVariables = {
          STARSHIP_CONFIG = ../pkgs/fish/starship.toml;
          SHELL = "fish";
        };

        systemPackages = with pkgs; let
          pythonPackages = py-pkgs: with py-pkgs; [virtualenv];
          python = python3.withPackages pythonPackages;
        in [
          age
          alejandra
          asciinema
          awscli2
          bat
          binutils
          bottom
          caddy
          clang-tools
          coppwr
          coreutils
          davinci-resolve
          deadnix
          direnv
          discord
          dnsutils
          dyff
          # element-desktop
          eza
          fd
          fennel
          ffmpeg
          file
          firefox
          fishPlugins.fzf-fish
          fzf
          gcc
          gdb
          gimp
          git
          gnumake
          gnupg
          go_1_23
          gopls
          gotools
          gptfdisk
          httpie
          hunspellDicts.en-us
          imagemagick
          imv
          inkscape
          ipcalc
          jless
          jq
          leiningenJDK11
          libreoffice
          lls
          lsof
          luajit_2_1
          luajit_2_1.pkgs.luacheck
          man-pages
          man-pages-posix
          mprocs
          mpv
          mupdf
          ncdu
          neofetch
          neovim
          nickel
          nil
          nmap
          nodePackages_latest.bash-language-server
          nodePackages_latest.typescript-language-server
          nodejs_22 # TODO replace with latest
          obsidian
          p7zip
          pamixer
          pavucontrol
          pciutils
          pgcli
          pinentry-gtk2
          playerctl
          procs
          pv
          pyright
          python
          restic
          ripgrep
          rlwrap
          rust-analyzer
          rust-bin.stable.latest.default
          rustybox
          shellcheck
          slack
          socat
          sops
          spotify
          starship
          statix
          sumneko-lua-language-server
          tcpdump
          tdesktop
          tmux-config
          tokei
          unixtools.xxd
          usbutils
          viddy
          watchexec
          wine
          wireguard-tools
          xplr
          zls
          zoom-us
          zoxide
        ];
      };

      fonts.packages = [
        pkgs.nerd-fonts.hack
        pkgs.nerd-fonts.noto
        pkgs.siji
        pkgs.unifont
      ];

      i18n.defaultLocale = "en_US.UTF-8";

      location = {
        provider = "manual";
        # https://location.services.mozilla.com/v1/geolocate?key=geoclue

        # Denver
        latitude = 39.6888;
        longitude = -105.156;
      };

      networking = {
        firewall = {
          # warning: Strict reverse path filtering breaks Tailscale exit node use
          # and some subnet routing setups. Consider setting
          # `networking.firewall.checkReversePath` = 'loose'
          checkReversePath = "loose";

          trustedInterfaces = ["tailscale0"];

          allowedTCPPorts = [
            31337 # netcat
          ];
          allowedUDPPorts = [
            21027 # syncthing local discovery
            31337 # netcat
          ];
        };

        dhcpcd.enable = false;
        networkmanager.enable = true;
      };

      nix = {
        extraOptions = ''
          experimental-features = nix-command flakes
        '';

        gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 30d";
        };
      };

      programs.dunst.enable = true;
      programs.command-not-found.enable = false;

      # use bash as interactive shell so that systemd emergency mode still
      # works, but immediately execute fish under most conditions
      programs.bash = {
        interactiveShellInit = ''
          if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
          then
            shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
            exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
          fi
        '';
      };

      programs.fish = {
        enable = true;
        shellInit = builtins.readFile ../pkgs/fish/config.fish;
      };

      programs.nix-index = {
        enable = true;
        enableZshIntegration = false;
        enableBashIntegration = false;
      };

      security.sudo.extraRules = [
        {
          commands = [
            {
              command = "ALL";
              options = ["SETENV" "NOPASSWD"];
            }
          ];
          groups = ["wheel"];
          host = "ALL";
          runAs = "ALL:ALL";
          users = [];
        }
      ];

      services.auto-cpufreq.enable = true;
      services.fstrim.enable = true;
      services.thermald.enable = true;

      services.pipewire = {
        enable = true;
        alsa.enable = true;
        pulse.enable = true;
        jack.enable = true;
      };

      # systemd.user.services.pipewire.environment = {
      #   PIPEWIRE_DEBUG = "D";
      # };

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql_14;
      };

      services.tailscale.enable = true;

      services.timesyncd.enable = false;
      services.chrony.enable = true;

      systemd.user.services.syncthing = {
        description = "Syncthing - Open Source Continuous File Synchronization";
        documentation = ["man:syncthing(1)"];
        startLimitIntervalSec = 60;
        startLimitBurst = 4;
        wantedBy = ["default.target"];
        enable = false;

        serviceConfig = {
          ExecStart = "${pkgs.syncthing}/bin/syncthing serve --no-browser --no-restart --logflags=0";
          Restart = "on-failure";
          RestartSec = 1;
          SuccessExitStatus = [3 4];
          RestartForceExitStatus = [3 4];

          # Hardening
          SystemCallArchitectures = ["native"];
          MemoryDenyWriteExecute = true;
          NoNewPrivileges = true;
        };
      };

      systemd.user.services.playerctld = {
        description = "Playerctl daemon to keep track of the active media player.";
        documentation = ["man:playerctld(1)"];
        startLimitIntervalSec = 60;
        startLimitBurst = 4;
        wantedBy = ["default.target"];

        serviceConfig = {
          ExecStart = "${pkgs.playerctl}/bin/playerctld";
          Restart = "on-failure";
          RestartSec = 1;
        };
      };

      systemd.user.services.rclipd = {
        description = "Network copy backend for tmux based on xsel.";
        wantedBy = ["graphical-session.target"];
        serviceConfig.ExecStart = "${pkgs.rustybox}/libexec/rclipd";
      };

      users.users."${cfg.primaryUser}" = {
        extraGroups = [
          "adbusers"
          "audio"
          "docker"
          "networkmanager"
          "vboxusers"
          "video"
          "wheel"
        ];
        isNormalUser = true;
        shell = pkgs.bash;
        uid = 1000;
      };

      virtualisation.docker.enable = true;
    };
  }
