{pkgs, ...}: {
  config = {
    systemd.services.newrelic-infra = {
      after = ["dbus.service" "syslog.target" "network.target" "init-keys.service"];
      wantedBy = ["multi-user.target"];
      description = "New Relic Infrastructure Agent";
      path = [pkgs.kmod];

      serviceConfig = {
        RuntimeDirectory = "newrelic-infra";
        Type = "simple";
        ExecStart = "${pkgs.newrelic-infra}/bin/newrelic-infra-service";
        MemoryLimit = "500M";
        Restart = "always";
        RestartSec = 20;
        EnvironmentFile = "/run/keys/newrelic-infra.env";

        Environment = ''
          NRIA_ENABLE_PROCESS_METRICS=true
        '';
      };

      unitConfig = {
        StartLimitInterval = 0;
        StartLimitBurst = 5;
      };
    };
  };
}
