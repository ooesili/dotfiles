import ./base.nix {
  pkgs = import <nixpkgs> {};
  config = {
    alacritty.font.size = "6.0";
    xmodmap.enable = true;
    xmodmap.swapAltSuper = false;
  };
}
