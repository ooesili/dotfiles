{ config, pkgs, lib, ... }:

{
  imports = [
    ./base.nix
  ];

  dotfiles = rec {
    desktop = {
      autoLoginUser = primaryUser;
      alacritty.font.size = "7.0";
      xmodmap.enable = true;
      xmodmap.swapAltSuper = false;
    };
    primaryUser = "ooesili";
    mpd = {
      dataDir = "/home/${primaryUser}/.local/share/mpd";
      musicDir = "/home/${primaryUser}/exthd/files/music";
      soundCard = "PCH";
      user = primaryUser;
    };
  };


  environment.systemPackages = with pkgs; [ xorg.xbacklight ];

  networking.hostName = "nixbook";
  networking.wireless.enable = true;

  # touchpad support
  services.xserver.libinput.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
