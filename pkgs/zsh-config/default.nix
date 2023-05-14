{
  stdenvNoCC,
  profileExtra ? "",
}: let
  ohMyZsh = builtins.fetchGit {
    name = "oh-my-zsh";
    url = "https://github.com/robbyrussell/oh-my-zsh.git";
    rev = "db94f60d342ba2be7dbe3bfd86f4edb335c2a6a7";
  };

  zshCustom = ./custom;
in
  stdenvNoCC.mkDerivation {
    name = "zsh-config-wrapped";
    src = ./.;
    inherit ohMyZsh profileExtra zshCustom;

    installPhase = ''
      mkdir -p $out/etc
      export zprofile="$out/etc/zprofile"

      substituteAll zshrc $out/etc/zshrc
      substituteAll zprofile $out/etc/zprofile
    '';
  }
