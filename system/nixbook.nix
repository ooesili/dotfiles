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

  services.mpd.musicDirectory = "/arch/home/ooesili/music";

  # touchpad support
  services.xserver.libinput.enable = true;
}
