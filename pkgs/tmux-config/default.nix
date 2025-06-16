{
  runCommand,
  makeWrapper,
  tmux,
  rustybox,
  stdenv,
  replaceVars,
  ...
}: let
  clipboard =
    if stdenv.hostPlatform.isDarwin
    then {
      copyCommand = "pbcopy";
      pasteCommand = "pbpaste";
    }
    else {
      copyCommand = "${rustybox}/bin/rclip copy --clipboard";
      pasteCommand = "${rustybox}/bin/rclip paste --clipboard";
    };

  configFile = replaceVars ./tmux.conf {
    inherit (clipboard) copyCommand pasteCommand;
  };
in
  runCommand "tmux-config-wrap" {
    buildInputs = [makeWrapper];
    meta.priority = (tmux.meta.priority or 0) + 1;
  } ''
    makeWrapper ${tmux}/bin/tmux $out/bin/tmux --add-flags "-f ${configFile}"
  ''
