{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.services.cloudflareDDNS;

  ddnsScript = pkgs.writeShellApplication {
    name = "cloudflare-ddns";

    runtimeInputs = [
      pkgs.curl
      pkgs.jq
    ];

    text = ''
      record_uri="https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records/$RECORD_ID"

      _curl() {
        curl --proto '=https' --tlsv1.2 --silent --show-error --fail "$@"
      }
      cf_curl() {
        _curl -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" "$@"
      }

      while true; do
        desired_ip="$(_curl https://www.cloudflare.com/cdn-cgi/trace | grep '^ip=' | cut -d= -f2)"
        actual_ip="$(cf_curl "$record_uri" | jq -r .result.content)"

        if [[ "$desired_ip" != "$actual_ip" ]]; then
          cf_curl -X PUT \
            -H 'Content-Type: application/json' \
            -d '{"type":"A","name":"home","ttl":1,"content":"'"$desired_ip"'"}' \
            "$record_uri" > /dev/null
          echo "updated from $actual_ip to $desired_ip"
        fi

        sleep 10s
      done
    '';
  };
in {
  options = {
    services.cloudflareDDNS = {
      enable = lib.mkOption {
        default = false;
        description = "Enable the CloudFlare DDNS scheduled job.";
        type = lib.types.bool;
      };

      environmentFile = lib.mkOption {
        default = "/run/keys/cloudflare-ddns.env";
        description = ''
          A file with environment variables to configure access to manage a
          CloudFlare DNS record. The file must be formatted to work with the
          EnvironmentFile= option described in systemd.exec(5). The required
          values are:
          - CLOUDFLARE_API_TOKEN
          - ZONE_ID
          - RECORD_ID
        '';
        type = lib.types.str;
      };
    };
  };

  config = {
    systemd.services.cloudflare-ddns = lib.mkIf cfg.enable {
      wantedBy = ["multi-user.target"];
      after = ["network.target" "init-keys.service"];

      serviceConfig = {
        ExecStart = "${ddnsScript}/bin/cloudflare-ddns";
        EnvironmentFile = cfg.environmentFile;
      };
    };
  };
}
