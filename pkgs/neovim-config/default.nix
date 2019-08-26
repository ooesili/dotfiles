{ config, lib, stdenvNoCC, ... }:

let
  inherit (lib.lists) last;
  inherit (lib.strings) removeSuffix;

  mkNeovimPlugin = plugin:
    let
      basename = path: last (builtins.split "/" path);
      pluginName = removeSuffix ".git" (basename plugin.url);
      version = builtins.substring 0 8 plugin.rev;
    in builtins.fetchGit {
      name = "neovim-plugin-${pluginName}-${version}";
      inherit (plugin) url rev;
    };

  neovimPluginRefs = lib.importJSON ./plugins.json;
  neovimPlugins = map mkNeovimPlugin neovimPluginRefs;

  pathogen = builtins.fetchGit {
    name = "vim-pathogen";
    url = https://github.com/tpope/vim-pathogen.git;
    rev = "06da921608b971fb47603671bcafdb2843992eb3";
  };

  zrupa = builtins.fetchGit {
    name = "zrupa";
    url = https://github.com/rupa/z.git;
    rev = "9d5a3fe0407101e2443499e4b95bca33f7a9a9ca";
  };

in stdenvNoCC.mkDerivation rec {
  name = "neovim-config";
  src = ./.;
  outputs = [ "out" ];
  inherit neovimPlugins;

  patchPhase = ''
    # sed -i 's:@@z\.sh:${zrupa}/z.sh:g' files/nvim/init.vim
  '';

  installPhase = ''
    mkdir -p $out/etc/xdg
    cp -R nvim $out/etc/xdg/
    cp -R ${pathogen}/autoload $out/etc/xdg/nvim/
    mkdir $out/etc/xdg/nvim/bundle
    ln -s $neovimPlugins $out/etc/xdg/nvim/bundle
  '';
}
