{
  stdenv,
  fetchFromGitHub,
  lib,
}:
stdenv.mkDerivation {
  pname = "mdloader";
  version = "1.0.7";

  src = fetchFromGitHub {
    owner = "Massdrop";
    repo = "mdloader";
    rev = "aeee18977d113d2f5d3a1df0dcdcea53d4f230b7";
    sha256 = "sha256-ydi9XMHztmechVEfXD5gT0Solk3D2CGAp69xvXK5iYk=";
  };

  installPhase = ''
    install -Dm 755 build/mdloader $out/bin/mdloader
  '';

  meta = {
    description = "Massdrop Firmware Loader - for CTRL / ALT / SHIFT / Rocketeer keyboards";
    homepage = "https://github.com/Massdrop/mdloader";
    license = lib.licenses.gpl3;
  };
}
