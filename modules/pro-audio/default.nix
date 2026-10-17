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
      pkgs.supercollider
      pkgs.bitwig-studio5
      pkgs.fire
      pkgs.paulstretch
      # pkgs.surge-XT
      pkgs.yabridge
      pkgs.wineWow64Packages.stable
      pkgs.winetricks
      pkgs.yabridgectl
      pkgs.vital-vst
    ];
  };
}
