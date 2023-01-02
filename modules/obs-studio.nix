{
  config,
  pkgs,
  lib,
  unstable,
  ...
}: {
  boot.extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
  boot.kernelModules = ["v4l2loopback"];
  environment.systemPackages = [unstable.obs-studio];
}
