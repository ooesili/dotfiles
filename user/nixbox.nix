import ./base.nix {
  pkgs = import <nixpkgs> {};
  config = {
    alacritty.font.size = "8.0";
    xmodmap.enable = false;
  };
}
