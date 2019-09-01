{ stdenvNoCC, substituteAll, profileExtra ? "" }:

let
  ohMyZsh = builtins.fetchGit {
    name = "oh-my-zsh";
    url = https://github.com/robbyrussell/oh-my-zsh.git;
    rev = "035d78120cb41297068967d3205a23bee22b9543";
  };

in stdenvNoCC.mkDerivation {
  name = "zsh-config-wrapped";
  src = ./.;
  inherit ohMyZsh profileExtra;

  installPhase = ''
    mkdir -p $out/etc
    substituteAll zshrc $out/etc/zshrc
    substituteAll zprofile $out/etc/zprofile
  '';
}
