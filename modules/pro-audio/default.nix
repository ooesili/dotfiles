{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./axefx2.nix
    ./jack.nix
  ];

  config = {
    environment.systemPackages = [
      pkgs.audacity
      pkgs.patchage
      pkgs.supercollider
      pkgs.bitwig-studio4
      pkgs.fire
      pkgs.paulstretch
      pkgs.surge-XT
      pkgs.vital-vst
    ];
  };
}
