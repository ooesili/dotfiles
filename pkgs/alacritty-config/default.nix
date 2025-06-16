{
  config,
  stdenvNoCC,
  makeWrapper,
  replaceVars,
  alacritty,
  ...
}: let
  # baseConfig = builtins.fromTOML (import ./alacritty.toml)

  configToml = replaceVars ./alacritty.toml config;
in
  stdenvNoCC.mkDerivation {
    name = "alacritty-config-wrapped";
    buildInputs = [makeWrapper];
    src = ./.;
    meta.priority = (alacritty.meta.priority or 0) + 1;

    inherit (config) fontSize;
    extraConfig = config.extraConfig or "";

    installPhase = ''
      mkdir -p $out/etc/xdg/configctl
      install -m 644 ${configToml} $out/etc/xdg/configctl/alacritty.toml
      echo "$extraConfig" >> $out/etc/xdg/configctl/alacritty.toml
      makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty \
        --add-flags '--config-file $XDG_RUNTIME_DIR/configctl/alacritty.toml'
    '';
  }
