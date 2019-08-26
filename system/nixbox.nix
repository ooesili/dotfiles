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
  services.mpd.musicDirectory = "/home/ooesili/exthd/files/music";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
