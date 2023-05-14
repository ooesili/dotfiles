{
  stdenv,
  requireFile,
  autoPatchelfHook,
  libGL,
  freetype,
  unzip,
  libsecret,
  glib,
  alsa-lib,
  curlWithGnuTls,
}: let
  srcName = "VitalInstaller.zip";
  version = "1.0.8";
in
  stdenv.mkDerivation {
    pname = "vital-vst";
    inherit version;

    src = requireFile {
      name = srcName;
      message = ''
        This nix expression requires that version ${version} of ${srcName} is
        already part of the store. Download the file from https://vital.audio/
        and add it to the nix store with `nix store add-file <FILE>`.
      '';
      sha256 = "sha256-CMKRo3ghVfGR55x3DHji47c2HxGKAPQAL0V0rdsFsaI=";
    };

    installPhase = ''
      # bin
      install -D vital $out/bin/vital

      # VST3
      mkdir -p $out/lib/vst3
      mv Vital.vst3 $out/lib/vst3

      # LV2
      mkdir -p $out/lib/lv2
      mv Vital.lv2 $out/lib/lv2

      # VST
      mkdir -p $out/lib/vst
      mv Vital.so $out/lib/vst
    '';

    buildInputs = [
      alsa-lib
      curlWithGnuTls.dev
      freetype.dev
      glib
      libGL
      libsecret
    ];

    nativeBuildInputs = [
      autoPatchelfHook
      unzip
    ];
  }
