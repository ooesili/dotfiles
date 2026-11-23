{
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation {
  pname = "kanagawa-tm-themes";
  version = "unstable";

  src = fetchFromGitHub {
    owner = "obergodmar";
    repo = "kanagawa-tmTheme";
    rev = "6a8f523f2f6f64f8e362f4ee59c0b610a1a3d2f7";
    hash = "sha256-FdIqdAtHfx5VElBK3WKULn2GTvZh2evfFLSEdQFqbpo=";
  };

  installPhase = ''
    mkdir -p "$out"
    cp -r ./*.tmTheme "$out"
  '';
}
