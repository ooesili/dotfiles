import ./base.nix {
  pkgs = import <nixpkgs> {};
  config = {
    alacritty.font.size = "7.0";
    xmodmap.enable = true;
    xmodmap.swapAltSuper = false;
  };
}
