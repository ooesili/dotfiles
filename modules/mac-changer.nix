{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.sec.macchanger;
in {
  options = {
    sec.macchanger = {
      enable = mkOption {
        default = false;
        description = ''
          Whether to enable MAC address randomization.
        '';
      };

      devices = mkOption {
        type = types.listOf types.str;
        default = [];
        example = literalExample ''
            [
              "eth0"
              "wlan0"
            ]
          '';
        description = ''
            List of devices to include for MAC address spoofing.
          '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.services = mkMerge (forEach cfg.devices (x:
      {
        "macchanger-${x}" = {
          description = "Randomize MAC address of ${x}";
          wants = [ "network-pre.target" ];
          wantedBy = [ "multi-user.target" ];
          before = [ "network-pre.target" ];
          bindsTo = [ "sys-subsystem-net-devices-${x}.device" ];
          after = [ "sys-subsystem-net-devices-${x}.device" ];
          script = ''
            get_current_mac_of_nic() {
              local mac
              mac="$(${pkgs.macchanger}/bin/macchanger "''${1}" | sed -n "s/^Current\s*MAC:\s*\([0-9a-f:]\+\)\s.*$/\1/p" || :)"
              if echo "''${mac}:" | grep -q "^\([0-9a-fA-F]\{2\}:\)\{6\}$"; then
                echo "''${mac}"
              fi
            }
            OLD_MAC="$(get_current_mac_of_nic "${x}")"
            # There is a 1/2^24 chance macchanger will randomly pick the real MAC
            # address. We try to making it really unlikely repeating it up to
            # three times. Theoretically speaking this leaks information about the
            # real MAC address at each occasion but actually leaking the real MAC
            # address will be more serious in practice.
            for i in 1 2 3; do
              ${pkgs.macchanger}/bin/macchanger -e "${x}" || true
              NEW_MAC="$(get_current_mac_of_nic "${x}")"
              if [ "''${OLD_MAC}" != "''${NEW_MAC}" ]; then
                break
              fi
            done
            '';
          serviceConfig.Type = "oneshot";
        };
      }
    ));
  };
}
