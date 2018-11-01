{ config, pkgs, lib, ... }:

{
  imports = [
    ./base.nix
  ];

  dotfiles = {
    primaryUser = "ooesili";
    soundCard = "PCH";
  };

  environment.systemPackages = with pkgs; [ xorg.xbacklight ];

  networking.hostName = "nixbook";
  networking.wireless.enable = true;

  # touchpad support
  services.xserver.libinput.enable = true;
}
