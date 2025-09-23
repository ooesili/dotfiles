{
  config,
  pkgs,
  ...
}: let
  init-keys = pkgs.writeShellApplication {
    name = "init-keys";
    runtimeInputs = [
      pkgs.bash
      pkgs.jq
    ];

    text = ''
      main() {
        write_secrets
        remove_extra_files
      }

      write_secrets() {
        jq -r \
          '.secrets|to_entries|map("echo \(.value|@sh) > /run/keys/\(.key).env")|.[]' \
          /etc/secrets.json |
          bash
      }

      remove_extra_files() {
        comm -13 <(desired_files) <(actual_files) | xargs rm -f
      }

      desired_files() {
        jq -r \
          '.secrets|keys|sort|map("/run/keys/\(.).env")|.[]' \
          /etc/secrets.json
      }

      actual_files() {
        ls /run/keys/*
      }

      main "$@"
    '';
  };

  update-keys = pkgs.writeShellApplication {
    name = "update-keys";

    text = ''
      main() {
        touch /etc/secrets.json
        chmod 0400 /etc/secrets.json
        cat > /etc/secrets.json

        if does_unit_exist init-keys.service; then
          sudo systemctl start init-keys.service
        fi
      }

      does_unit_exist() {
        local unit=$1
        systemctl list-unit-files "$unit" | grep -qF '1 unit files listed.'
      }

      main "$@"
    '';
  };
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../modules/newrelic-infra.nix
    ../../modules/trusts.nix
    ../../modules/cloudflare-ddns
  ];

  boot = {
    # Use the extlinux boot loader. (NixOS wants to enable GRUB by default)
    loader.grub.enable = false;
    # Enables the generation of /boot/extlinux/extlinux.conf
    loader.generic-extlinux-compatible.enable = true;

    kernelPackages = pkgs.linuxPackages_latest;
  };

  console = {
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

  networking = {
    hostName = "pinix";
    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    interfaces.eth0.useDHCP = true;
    useDHCP = false;

    firewall.enable = true;
    firewall.allowedUDPPorts = [
      51820 # wireguard
    ];
    firewall.allowedTCPPorts = [80 443];
  };

  nixpkgs = {
    overlays = [
      (final: _prev: {
        firmwareLinuxNonfree = final.firmwareLinuxNonfree.overrideAttrs (_oldAttrs: {
          version = "2020-12-18";
          src = pkgs.fetchgit {
            url = "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git";
            rev = "b79d2396bc630bfd9b4058459d3e82d7c3428599";
            sha256 = "1rb5b3fzxk5bi6kfqp76q1qszivi0v1kdz1cwj2llp5sd9ns03b5";
          };
          outputHash = "1p7vn2hfwca6w69jhw5zq70w44ji8mdnibm1z959aalax6ndy146";
        });
      })
    ];

    config.allowUnfree = true;
  };

  environment.systemPackages = let
    sslpsk = pkgs.python3Packages.buildPythonPackage {
      pname = "sslpsk";
      version = "2020-01-29";

      src = pkgs.fetchFromGitHub {
        owner = "drbild";
        repo = "sslpsk";
        rev = "d88123a75786953f82f5e25d6c43d9d9259acb62";
        sha256 = "sha256-RqaZLtRMzYJPKXBBsw1alujGyqWAQRSQLPyAR8Zi6t4=";
      };

      buildInputs = [
        pkgs.openssl
        pkgs.pkg-config
      ];

      meta = {
        description = " Adds TLS-PSK support to the Python ssl package ";
        homepage = "https://github.com/drbild/sslpsk";
        license = pkgs.lib.licenses.asl20;
      };
    };

    python = pkgs.python3.withPackages (ps: [
      ps.paho-mqtt
      ps.tornado
      ps.pycryptodomex
      sslpsk
    ]);
  in [
    update-keys

    pkgs.htop
    pkgs.tcpdump
    pkgs.vim

    ### tuya-convert ###
    pkgs.dnsmasq
    pkgs.git
    pkgs.haveged
    pkgs.hostapd
    pkgs.iw
    pkgs.mosquitto
    pkgs.openssl
    pkgs.pkg-config
    pkgs.screen
    python
  ];

  nix.settings.trusted-users = ["root" "@wheel"];

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

  services.caddy = {
    enable = true;
    extraConfig = ''
      git.ooesili.me {
        reverse_proxy unix//run/gitea/gitea.sock
      }

      nixbox.ooesili.me {
        handle_path /static/* {
          reverse_proxy 100.121.171.91:9080
        }
        reverse_proxy 100.121.171.91:8080
      }

      trefoil.ooesili.me {
        reverse_proxy 100.121.171.91:8000
      }

      nitter.ooesili.me {
        reverse_proxy localhost:${builtins.toString config.services.nitter.server.port}
      }

      registry.ooesili.me {
        reverse_proxy 100.121.171.91:5000
      }
    '';
  };

  services.cloudflareDDNS.enable = true;

  services.nitter = {
    enable = true;
    preferences.mp4Playback = true;
    preferences.hlsPlayback = true;
  };

  services.tailscale.enable = true;

  systemd.services.init-keys = {
    wantedBy = ["multi-user.target"];
    description = "Unpack secrets into /run/keys";

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${init-keys}/bin/init-keys";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

  time.timeZone = "UTC";

  users.users.ooesili = {
    isNormalUser = true;
    extraGroups = ["wheel"]; # Enable ‘sudo’ for the user.
  };
}
