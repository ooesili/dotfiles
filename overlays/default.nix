let
  overlay = final: prev: {
    audio-mode = final.callPackage ../pkgs/audio-mode.nix {};
    keymap-us-capsctrl = final.callPackage ../pkgs/keymap-us-capsctrl {};
    mdloader = final.callPackage ../pkgs/mdloader.nix {};
    newrelic-infra = final.callPackage ../pkgs/newrelic-infra {};
    rustybox = final.callPackage ../tools/rustybox/package.nix {};
    vital-vst = final.callPackage ../pkgs/vst/vital.nix {};

    discord = prev.discord.overrideAttrs (_: let
      version = "0.0.60";
    in {
      inherit version;
      src = prev.fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "sha256-hu1+/z/ZtHoobjHF+pgNm040r4LQJUTnpZ06RNERFr8=";
      };
    });

    bitwig-studio5 = prev.bitwig-studio5.overrideAttrs (oldAttrs: let
      version = "5.1.9";
    in {
      inherit version;
      src = prev.fetchurl {
        url = "https://www.bitwig.com/dl/Bitwig%20Studio/${version}/installer_linux/";
        hash = "sha256-J5kLqXCMnGb0ZMhES6PQIPjN51ptlBGj4Fy8qSzJ6Qg=";
      };
    });

    # The version of libnss exported by the discord wrapper is incompatible with
    # the one firefox expects.
    firefox = let
      firefox-wrapped =
        prev.runCommand "firefoxNoLdLibPath" {
          buildInputs = [prev.makeWrapper];
        } ''
          makeWrapper ${prev.firefox}/bin/firefox $out/bin/firefox --unset LD_LIBRARY_PATH
        '';
    in
      prev.symlinkJoin {
        name = "firefox";
        paths = [firefox-wrapped prev.firefox];
      };

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
  };
in [
  overlay
  (import ./neovim.nix)
]
