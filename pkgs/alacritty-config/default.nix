{ config, substituteAll, makeWrapper, runCommand, alacritty, ... }:

let
  configFile = substituteAll {
  name = "alacritty-config";
  src = ./alacritty.yml;
  fontSize = config.alacritty.font.size;
};

in runCommand "alacritty-config-wrapped" {
  buildInputs = [ makeWrapper ];
  meta.priority = (alacritty.meta.priority or 0) + 1;
} ''
  makeWrapper ${alacritty}/bin/alacritty $out/bin/alacritty \
    --add-flags "--config-file ${configFile}"
''
