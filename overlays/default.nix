let
  overlay = final: prev: {
    audio-mode = final.callPackage ../pkgs/audio-mode.nix {};
    keymap-us-capsctrl = final.callPackage ../pkgs/keymap-us-capsctrl {};
    mdloader = final.callPackage ../pkgs/mdloader.nix {};
    newrelic-infra = final.callPackage ../pkgs/newrelic-infra {};
    nopt = final.callPackage ../pkgs/nopt.nix {};
    rustybox = final.callPackage ../tools/rustybox/package.nix {};
    godoc = final.callPackage ../pkgs/godoc.nix {};
    polybar = prev.polybar.override {
      i3Support = true;
      pulseSupport = true;
    };

    discord = prev.discord.overrideAttrs (_: let
      version = "0.0.23";
    in {
      inherit version;
      src = prev.fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "sha256-KIwAWQHyv1oRSAIeHSL9KXQZ1DxwsPmJggq3KqefqkQ=";
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

    direnv = prev.direnv.overrideAttrs (oldAttrs: {
      patches = [./direnv-use-nix-no-trace.patch];
    });

    gopls = prev.gopls.override {
      buildGoModule = prev.buildGo119Module;
    };

    sops = prev.sops.overrideAttrs (oldAttrs: {
      patches = [./sops-yaml-indent.patch];
    });

    zdirs = final.writeTextFile {
      name = "zdirs";
      executable = true;
      destination = "/bin/zdirs";
      text = ''
        #!${final.runtimeShell}
        source ${final.z}/z.sh
        _z 2>&1 | sed 's/^[0-9.]*[[:blank:]]*//' | tac
      '';

      meta.mainProgram = "zdirs";
    };

    rofi = let
      rofiThemeBase16 = builtins.fetchGit {
        name = "base46-rofi-theme";
        url = https://github.com/0xdec/base16-rofi.git;
        rev = "a7e7be0cb5812243f23cd4607eab11ce4cca7774";
      };
    in
      prev.runCommand "rofi-config-wrapped" {
        buildInputs = [prev.makeWrapper];
      } ''
        makeWrapper ${prev.rofi}/bin/rofi $out/bin/rofi \
          --add-flags "-theme ${rofiThemeBase16}/themes/base16-default-dark.rasi"
      '';

    z = builtins.fetchGit {
      name = "zrupa";
      url = https://github.com/rupa/z.git;
      rev = "125f4dc47e15891739dd8262d5b23077fe8fb9ab";
    };
  };
in [
  overlay
  (import ./neovim.nix)
]
