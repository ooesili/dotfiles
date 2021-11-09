{ lib, rustPlatform, playerctl, xsel }:

rustPlatform.buildRustPackage {
  pname = "rustybox";
  version = "0.1.0";
  cargoSha256 = "sha256-jofe5xGGwf90nZGSgj8LETM29KBng9swXA53D8D6tR0=";

  src = lib.cleanSource ./.;
  postInstall = ''
    mkdir -p $out/libexec

    ln $out/bin/rustybox $out/bin/configctl
    ln $out/bin/rustybox $out/bin/mediactl
    ln $out/bin/rustybox $out/bin/rclip
    ln $out/bin/rustybox $out/libexec/batteryd
    ln $out/bin/rustybox $out/libexec/rclipd
    rm $out/bin/rustybox
  '';

  PLAYERCTL_BIN = "${playerctl}/bin/playerctl";
  XSEL_BIN = "${xsel}/bin/xsel";
}
