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

  cargo-toml = builtins.fromTOML (builtins.readFile ./crates/rustybox/Cargo.toml);
  deps = cargo-toml.dependencies;
  crates =
    builtins.filter
    (name: let d = deps.${name}; in d ? path)
    (builtins.attrNames deps);
  commands =
    map (
      name: let
        manifest = builtins.fromTOML (builtins.readFile ./crates/${name}/Cargo.toml);
      in {
        inherit name;
        inherit (manifest.package.metadata.rustybox) outdir;
      }
    )
    crates;
in
  rustPlatform.buildRustPackage {
    pname = "rustybox";
    version = "0.1.0";
    cargoHash = "sha256-waeDzPoMyq3wIsfw2T/MXyzRGWl1ydB1jMnA9nZpveY=";

    src = lib.cleanSource ./.;
    buildInputs = [makeWrapper];

    postInstall = ''
      mkdir -p $out/bin $out/libexec

      wrapProgram $out/bin/rustybox --prefix PATH : ${lib.makeBinPath pathDepends}
      ${lib.concatMapStringsSep "\n" (
          cmd: "ln -s $out/bin/rustybox $out/${cmd.outdir}/${cmd.name}"
        )
        commands}
    '';
  }
