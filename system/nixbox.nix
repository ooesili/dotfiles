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
}
