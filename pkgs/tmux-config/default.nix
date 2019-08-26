{ config, lib, runCommand, makeWrapper, tmux, ... }:

runCommand "tmux-config-wrap" {
  buildInputs = [ makeWrapper ];
  meta.priority = tmux.meta.priority or 0;
} ''
  makeWrapper ${tmux}/bin/tmux $out/bin/tmux --add-flags "-f ${./tmux.conf}"
''
