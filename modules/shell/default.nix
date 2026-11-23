{
  config,
  pkgs,
  lib,
  ...
}: {
  config = {
    environment.variables = {
      STARSHIP_CONFIG = "${./starship.toml}";
      # TODO: delete this if nothing goes wrong
      # SHELL = "fish";
    };

    environment.systemPackages = [
      pkgs.fzf
      pkgs.starship
      pkgs.tmux-config
      pkgs.zoxide
    ];

    environment = {
      shells = [config.programs.fish.package];
      etc."bat/config".source = ./bat.conf;
      variables.BAT_CACHE_PATH = "${pkgs.bat-cache}";
    };

    programs.fish = {
      enable = true;
      shellInit = builtins.readFile ./config.fish;

      # Skip defaults that clobber our own. I can remove this if I put my
      # aliases here, as they will be interpolated into /etc/fish/config.fish
      # in the right place.
      shellAliases = {
        l = null;
        ll = null;
        ls = null;
      };
    };

    programs.direnv.enable = true;

    # use bash as interactive shell so that systemd emergency mode still
    # works, but immediately execute fish under most conditions
    programs.bash = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      interactiveShellInit = ''
        if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
        then
          shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
          exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
        fi
      '';
    };
  };
}
