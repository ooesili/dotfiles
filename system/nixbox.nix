{
  imports = [
    ./base.nix
  ];

  boot.extraModprobeConfig = ''
    blacklist snd_hda_intel
  '';

  dotfiles = rec {
    cloudflareDDNS = {
      enable = true;
      environmentFile = "/etc/secrets/cloudflare-ddns";
    };
    desktop = {
      autoLoginUser = primaryUser;
      alacritty.font.size = "9.0";
      xmodmap.enable = false;
    };
    primaryUser = "ooesili";
    mpd = {
      dataDir = "/home/${primaryUser}/.local/share/mpd";
      musicDir = "/home/${primaryUser}/exthd/files/music";
      soundCard = "Multibit";
      user = primaryUser;
    };
  };

  networking.hostName = "nixbox";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
