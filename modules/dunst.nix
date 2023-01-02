{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.programs.dunst;
in {
  options.programs.dunst = {
    enable = lib.mkOption {
      description = "Enable the dunst systemd user service.";
      type = lib.types.bool;
      default = false;
    };

    package = lib.mkOption {
      description = "Which dunst package to use.";
      type = lib.types.package;
      default = pkgs.dunst;
      defaultText = "pkgs.dunst";
    };
  };

  config = {
    environment.systemPackages = lib.mkIf cfg.enable [pkgs.dunst];

    systemd.user.services.dunst = lib.mkIf cfg.enable {
      description = "Dunst, a lightweight and customizable notification daemon.";
      documentation = ["man:dunst(1)"];
      partOf = ["graphical-session.target"];

      serviceConfig = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${cfg.package}/bin/dunst";
      };
    };
  };
}
