{
  pkgs,
  config,
  ...
}: let
  gitea-restore = pkgs.writeShellApplication {
    name = "gitea-restore";
    runtimeInputs = [
      config.services.gitea.package
      pkgs.p7zip
      pkgs.sqlite
    ];

    text = ''
      if [[ "$UID" != 0 ]]; then
        echo >&2 "error: $0 must be run as root"
      fi
      if [[ $# -lt 1 ]]; then
        echo >&2 "error: no gitea dump archive file given"
      fi
      backup_dir=$1; shift

      gitea_user=${config.services.gitea.user}
      state_dir=${config.services.gitea.stateDir}
      database_path=${config.services.gitea.database.path}

      tempdir=$(mktemp --directory)
      trap 'rm -rf $tempdir' EXIT
      7z x -o"$tempdir" "$backup_dir"

      sudo systemctl stop gitea.service

      cp -R "$tempdir"/data/* "$state_dir/data"
      cp -R "$tempdir"/repos/* "$state_dir/repositories"
      cp -R "$tempdir"/custom/* "$state_dir/custom"
      chown -R "$gitea_user" /var/lib/gitea

      rm "$database_path"
      # shellcheck disable=SC2024
      sudo -u "$gitea_user" sqlite3 "$database_path" < "$tempdir/gitea-db.sql"

      sudo systemctl start gitea.service
    '';
  };
in {
  environment.systemPackages = [
    gitea-restore
  ];

  services.gitea = {
    dump.enable = true;
    enable = true;
    enableUnixSocket = true;
    rootUrl = "https://git.ooesili.me";
    settings.service.DISABLE_REGISTRATION = true;
    settings.ui.DEFAULT_THEME = "arc-green";
  };
}
