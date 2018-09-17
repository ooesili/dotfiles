{
  imports = [
    ./base.nix
  ];

  dotfiles = {
    primaryUser = "ooesili";
    soundCard = "PCH";
  };

  networking.hostName = "nixbook";
  networking.wireless.enable = true;
}
