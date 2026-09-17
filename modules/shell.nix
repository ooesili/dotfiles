{
  config,
  pkgs,
  lib,
  ...
}: {
  config = {
    environment.variables = {
      STARSHIP_CONFIG = "${../pkgs/fish/starship.toml}";
      # TODO: delete this if nothing goes wrong
      # SHELL = "fish";
    };

    environment.systemPackages = [
      pkgs.fzf
      pkgs.starship
      pkgs.tmux-config
      pkgs.zoxide
    ];

    environment.shells = [config.programs.fish.package];

    programs.fish = {
      enable = true;
      shellInit = builtins.readFile ../pkgs/fish/config.fish;
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
