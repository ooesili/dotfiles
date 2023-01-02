{
  config,
  lib,
  pkgs,
  unstable,
  options,
  ...
}: {
  imports = [
    ../modules/trusts.nix
  ];

  sdImage.compressImage = false;
  nix.settings.trusted-users = ["root" "@wheel"];

  users.users.ooesili = {
    extraGroups = ["wheel"];
    isNormalUser = true;
  };

  system.stateVersion = "22.05";
}
