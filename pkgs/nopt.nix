{ writeShellApplication, writeText, nixos-option }:

let compat = writeText "nopt-compat" ''
  let flake = import (builtins.getEnv "FLAKE_PATH");
  in flake.nixosConfigurations.''${builtins.getEnv "HOSTNAME"}
'';

in writeShellApplication {
  name = "nopt";
  runtimeInputs = [ nixos-option ];

  text = ''
    export FLAKE_PATH="$HOME/sync/dotfiles/nix-config"

    if [[ "''${1:-}" = "-h" || "''${1:-}" = "--hostname" ]]; then
      export HOSTNAME=$2
      shift 2
    else
      HOSTNAME="$(cat /etc/hostname)"
      export HOSTNAME
    fi

    if [[ $# -lt 1 ]]; then
      echo "Usage: nopt [option…]"
      exit 1
    fi

    nixos-option -I nixpkgs=${compat} "$@"
  '';
}
