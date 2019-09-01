{ config, lib, runCommand, makeWrapper, tmux, substituteAll, copyCommand, pasteCommand, ... }:

let
  configFile = substituteAll {
    name = "tmux.conf";
    src = ./tmux.conf;
    copyCommand = copyCommand;
    pasteCommand = pasteCommand;
  };

in runCommand "tmux-config-wrap" {
  buildInputs = [ makeWrapper ];
  meta.priority = (tmux.meta.priority or 0) + 1;
} ''
  makeWrapper ${tmux}/bin/tmux $out/bin/tmux --add-flags "-f ${configFile}"
''
