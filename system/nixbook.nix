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

  # touchpad support
  services.xserver.libinput.enable = true;
}
