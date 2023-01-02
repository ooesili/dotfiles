{
  config,
  lib,
  stdenvNoCC,
  makeWrapper,
  alacritty,
  ...
}: let
  indentStr = with lib;
    indent: str: let
      lines = lib.splitString "\n" (removeSuffix "\n" str);
      indentedLines = map (s: indent + s) lines;
    in
      concatStringsSep "\n" indentedLines;
in
  stdenvNoCC.mkDerivation {
    name = "alacritty-config-wrapped";
    buildInputs = [makeWrapper];
    src = ./.;
    meta.priority = (alacritty.meta.priority or 0) + 1;

    inherit (config) fontSize;
    extraKeyBindings =
      if config ? extraKeyBindings
      then indentStr "  " config.extraKeyBindings
      else "";

    installPhase = ''
      mkdir -p $out/etc
      substituteAll alacritty.yml $out/etc/alacritty.yml
      makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty \
        --add-flags "--config-file $out/etc/alacritty.yml"
    '';
  }
