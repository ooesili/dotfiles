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
      musicDir = "/media/exthd/files/music/sorted";
      soundCard = "Multibit";
      user = primaryUser;
    };
  };

  networking.hostName = "nixbox";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
