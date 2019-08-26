import ./base.nix {
  pkgs = import <nixpkgs> {};
  config = {
    alacritty.font.size = "9.0";
    xmodmap.enable = false;
  };
}
