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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
