import ./base.nix {
  pkgs = import <nixpkgs> {};
  config = {
    xmodmap.enable = false;
  };
}
