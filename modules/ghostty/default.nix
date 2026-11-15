{
  pkgs,
  lib,
  config,
  ...
}: let
  configPath = "xdg/ghostty/config.ghostty";
in {
  environment.systemPackages = [
    pkgs.ghostty
  ];

  services.dbus.packages = [pkgs.ghostty];

  environment.etc.${configPath}.source = ./config.ghostty;

  systemd.user.services.ghostty = {
    description = "Ghostty Terminal Emulator";

    after = ["sway-session.target" "dbus.socket"];
    requires = ["dbus.socket"];
    wantedBy = ["sway-session.target"];

    serviceConfig = {
      Type = "notify-reload";
      ReloadSignal = "SIGUSR2";
      BusName = "com.mitchellh.ghostty";
      ExecStart = "${lib.getExe pkgs.ghostty} --gtk-single-instance=true --initial-window=false --quit-after-last-window-closed=false --config-file=/etc/${configPath}";
    };

    reloadIfChanged = true;
    reloadTriggers = [./config.ghostty];
    path = [config.programs.fish.package];
  };
}
