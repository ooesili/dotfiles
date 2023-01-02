{
  config,
  lib,
  runCommand,
  makeWrapper,
  tmux,
  substituteAll,
  copyCommand,
  pasteCommand,
  zsh,
  ...
}: let
  configFile = substituteAll {
    name = "tmux.conf";
    src = ./tmux.conf;
    inherit copyCommand pasteCommand zsh;
  };
in
  runCommand "tmux-config-wrap" {
    buildInputs = [makeWrapper];
    meta.priority = (tmux.meta.priority or 0) + 1;
  } ''
    makeWrapper ${tmux}/bin/tmux $out/bin/tmux --add-flags "-f ${configFile}"
  ''
