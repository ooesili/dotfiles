{mnw}: final: prev: {
  audio-mode = final.callPackage ../pkgs/audio-mode.nix {};
  axefx2-firmware = final.callPackage ../pkgs/axefx2-firmware.nix {};
  keymap-us-capsctrl = final.callPackage ../pkgs/keymap-us-capsctrl {};
  mdloader = final.callPackage ../pkgs/mdloader.nix {};
  rustybox = final.callPackage ../tools/rustybox/package.nix {};
  vital-vst = final.callPackage ../pkgs/vst/vital.nix {};

  bitwig-studio5 = prev.bitwig-studio5.overrideAttrs (oldAttrs: {
    nativeBuildInputs = [final.makeWrapper];
    # run on DIPSLAY :1 which uses xwayland-satellite to get around framework HiDPI issue
    postFixup = ''
      wrapProgram $out/bin/bitwig-studio --set DISPLAY :1
    '';
  });

  bitwig-studio5-unwrapped = prev.bitwig-studio5-unwrapped.overrideAttrs (
    _oldAttrs: let
      version = "5.1.9";
    in {
      inherit version;
      src = prev.fetchurl {
        name = "bitwig-studio-${version}.deb";
        url = "https://www.bitwig.com/dl/Bitwig%20Studio/${version}/installer_linux/";
        hash = "sha256-J5kLqXCMnGb0ZMhES6PQIPjN51ptlBGj4Fy8qSzJ6Qg=";
      };
    }
  );

  tmux-config = prev.callPackage ../pkgs/tmux-config {};

  direnv = prev.direnv.overrideAttrs (_oldAttrs: {
    patches = [./direnv-use-nix-no-trace.patch];
  });

  slack = final.symlinkJoin {
    name = "slack-no-wayland";
    paths = [prev.slack];
    buildInputs = [final.makeWrapper];
    postBuild = ''
      wrapProgram $out/bin/slack --unset NIXOS_OZONE_WL
      rm $out/share/applications/slack.desktop
      substitute \
        ${prev.slack}/share/applications/slack.desktop \
        $out/share/applications/slack.desktop \
        --replace ${prev.slack}/bin $out/bin
    '';
  };

  neovim = mnw.lib.wrap prev ../neovim;

  kanagawa = prev.callPackage ../pkgs/kanagawa {};

  # Discord is always opening chromium for some reason so this is a silly work
  # around.
  discord = let
    redirect = prev.writeShellScriptBin "chromium" ''${final.firefox}/bin/firefox "$@"'';
    binPath = prev.lib.makeBinPath [redirect];
  in
    prev.symlinkJoin {
      name = "discord-wrapped";
      paths = [prev.discord];
      buildInputs = [prev.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/discord --prefix PATH : ${binPath}
        wrapProgram $out/bin/Discord --prefix PATH : ${binPath}
      '';
    };

  leiningenJDK11 = prev.leiningen.override {jdk = final.openjdk11;};

  kanagawa-tm-theme = final.callPackage ../pkgs/kanagawa/tm-theme.nix {};
  bat-cache = final.callPackage ../pkgs/bat-cache.nix {};

  # delta does not respect $BAT_CACHE_PATH and is hard coded to look in
  # $XDG_CACHE_HOME, so we set it here.
  delta = prev.symlinkJoin {
    name = "delta-wrapped";
    paths = [prev.delta];
    buildInputs = [prev.makeWrapper];
    postBuild = ''
      mkdir -p $out/cache
      ln -s ${final.bat-cache} $out/cache/bat
      wrapProgram $out/bin/delta --set XDG_CACHE_HOME "$out/cache"
    '';
  };
}
