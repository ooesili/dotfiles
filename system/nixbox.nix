{
  imports = [
    ./base.nix
  ];

  boot.extraModprobeConfig = ''
    blacklist snd_hda_intel
  '';

  dotfiles = {
    primaryUser = "ooesili";
    soundCard = "Multibit";
  };

  networking.hostName = "nixbox";
  nixpkgs.config.virtualbox.enableExtensionPack = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  virtualisation.virtualbox.host.enable = true;
}
