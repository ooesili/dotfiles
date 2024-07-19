{pkgs, ...}: {
  config = {
    boot.kernelParams = ["threadirq"];

    environment.systemPackages = [
      pkgs.qjackctl
    ];

    # powerManagement.cpuFreqGovernor = "performance";

    security = {
      pam.loginLimits = [
        {
          domain = "@audio";
          item = "memlock";
          type = "-";
          value = "unlimited";
        }
        {
          domain = "@audio";
          item = "rtprio";
          type = "-";
          value = "99";
        }
        {
          domain = "@audio";
          item = "nofile";
          type = "soft";
          value = "99999";
        }
        {
          domain = "@audio";
          item = "nofile";
          type = "hard";
          value = "99999";
        }
      ];
      rtkit.enable = true;
    };

    services.das_watchdog.enable = true;
  };
}
