{
  runCommand,
  makeWrapper,
  tmux,
  rustybox,
  stdenv,
  replaceVars,
  kanagawa,
  writeText,
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

  myConfig = replaceVars ./tmux.conf {
    inherit (clipboard) copyCommand pasteCommand;
  };

  configFile = writeText "tmux.conf" ''
    source-file ${kanagawa}/kanagawa-tmux-wave.conf
    source-file ${myConfig}
  '';
in
  runCommand "tmux-config-wrap" {
    buildInputs = [makeWrapper];
    meta.priority = (tmux.meta.priority or 0) + 1;
  } ''
    makeWrapper ${tmux}/bin/tmux $out/bin/tmux --add-flags "-f ${configFile}"
  ''
