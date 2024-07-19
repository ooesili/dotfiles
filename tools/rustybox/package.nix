{
  lib,
  rustPlatform,
  playerctl,
  xsel,
  makeWrapper,
  wl-clipboard,
}: let
  pathDepends = [
    playerctl
    wl-clipboard
    xsel
  ];
in
  rustPlatform.buildRustPackage {
    pname = "rustybox";
    version = "0.1.0";
    cargoSha256 = "sha256-VQ56nSinUP2wf+JIHAmack60gT8bvN8p80Hw7iVkZzE=";

    src = lib.cleanSource ./.;
    buildInputs = [makeWrapper];

    postInstall = ''
      mkdir -p $out/libexec

      wrapProgram $out/bin/rustybox --prefix PATH : ${lib.makeBinPath pathDepends}
      ln $out/bin/rustybox $out/bin/configctl
      ln $out/bin/rustybox $out/bin/mediactl
      ln $out/bin/rustybox $out/bin/rclip
      ln $out/bin/rustybox $out/libexec/batteryd
      ln $out/bin/rustybox $out/libexec/rclipd
      rm $out/bin/rustybox
    '';
  }
