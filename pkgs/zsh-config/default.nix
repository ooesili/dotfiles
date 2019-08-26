{ stdenvNoCC }:

let
  ohMyZsh = builtins.fetchGit {
    url = https://github.com/robbyrussell/oh-my-zsh.git;
    rev = "035d78120cb41297068967d3205a23bee22b9543";
  };

in stdenvNoCC.mkDerivation {
  name = "dotfiles-pkg-zsh";
  src = ./.;
  outputs = [ "out" ];

  patchPhase = ''
    sed -i 's:@@ohMyZsh:${ohMyZsh}:g' zshrc
    sed -i 's:@@ohMyZsh:${ohMyZsh}:g' zshrc
  '';

  installPhase = ''
    mkdir -p $out/etc
    cp zprofile $out/etc/zprofile
    cp zshrc $out/etc/zshrc
  '';
}
