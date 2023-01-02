{
  config,
  pkgs,
  lib,
  ...
}: let
  populator = pkgs.writeShellApplication {
    name = "tailscale-dns-populator";

    runtimeInputs = [
      pkgs.jq
      pkgs.tailscale
    ];

    text = ''
      status="$(tailscale status --json)"
      backend_state="$(echo "$status" | jq -r .BackendState)"

      if [[ $backend_state != Running ]]; then
        echo "tailscale is not running, doing nothing"
        exit 0
      fi

      echo "tailscale is running, updating hosts file"
      jq -r '[.Self, .Peer[]][] | "\(.TailscaleIPs[]) \(.HostName).wg"' \
        <<< "$status" \
        > /var/lib/tailscale-hosts
    '';
  };
in
  lib.mkIf config.services.tailscale.enable {
    networking.nameservers = ["127.0.0.1"];

    services.coredns = {
      enable = true;
      config = ''
        .:53 {
          hosts /var/lib/tailscale-hosts {
            fallthrough
          }

          forward . tls://1.1.1.1 tls://1.0.0.1 {
            tls_servername cloudflare-dns.com
            health_check 5s
          }
        }
      '';
    };

    systemd.services.tailscale-dns-populator = {
      description = "Tailscale DNS populator.";

      serviceConfig = {
        ExecStart = "${populator}/bin/tailscale-dns-populator";
        Type = "oneshot";
      };

      startAt = "*:*:0/30";
    };
  }
