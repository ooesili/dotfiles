{
  buildGo119Module,
  fetchgit,
}:
buildGo119Module {
  pname = "godoc";
  version = "2022-07-26";

  src = fetchgit {
    rev = "39a4e36475d99fd719f5d62f870e662e057c7ad2";
    url = "https://go.googlesource.com/tools";
    sha256 = "sha256-8++fDXTWHPRr0VFD0JETi/nkrHlfUfOColf5V0YNWcU=";
  };
  vendorSha256 = "sha256-XpC3F6BimOja8Jv/MbbLaAdqPKipDY5gFZ8c+W2XoEM=";

  subPackages = "cmd/godoc";
  doCheck = false;
}
