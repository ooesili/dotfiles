{
  config,
  stdenvNoCC,
  makeWrapper,
  alacritty,
  ...
}:
stdenvNoCC.mkDerivation {
  name = "alacritty-config-wrapped";
  buildInputs = [makeWrapper];
  src = ./.;
  meta.priority = (alacritty.meta.priority or 0) + 1;

  inherit (config) fontSize;
  extraConfig = config.extraConfig or "";

  installPhase = ''
    mkdir -p $out/etc/xdg/configctl
    substituteAll alacritty.toml $out/etc/xdg/configctl/alacritty.toml
    echo "$extraConfig" >> $out/etc/xdg/configctl/alacritty.toml
    makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty \
      --add-flags '--config-file $XDG_RUNTIME_DIR/configctl/alacritty.toml'
  '';
}
