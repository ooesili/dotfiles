{ stdenv, fetchFromGitHub, lib }:

stdenv.mkDerivation {
  pname = "mdloader";
  version = "1.0.6";

  src = fetchFromGitHub {
    owner = "Massdrop";
    repo = "mdloader";
    rev = "e4f977416994f54ca3f8d2e72f2b225d52f7c42e";
    sha256 = "sha256-/hRS6T+/cy1QQfFAsBYdZjooDDCBP9ztRn1GtapVQMY=";
  };

  installPhase = ''
    install -Dm 755 build/mdloader $out/bin/mdloader
  '';

  meta = {
    description = "Massdrop Firmware Loader - for CTRL / ALT / SHIFT / Rocketeer keyboards";
    homepage = "https://github.com/Massdrop/mdloader";
    license = lib.licenses.gpl3;
    maintainers = [ lib.maintainers.eyjhb ];
  };
}
