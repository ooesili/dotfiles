{ config, pkgs, lib, ... }:

{
  imports = [
    ./axefx2.nix
    ./jack.nix
  ];

  config = {
    environment.systemPackages = with pkgs; [
      audacity
      patchage
      supercollider
    ];
  };
}
