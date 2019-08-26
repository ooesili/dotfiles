import ./base.nix {
  pkgs = import <nixpkgs> {};
  config = {
    alacritty.font.size = "9.0";

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "19.03"; # Did you read the comment?

    xmodmap.enable = false;
  };
}
