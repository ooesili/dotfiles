{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.dotfiles.mpd;

  ncmpcppWrapped =
    pkgs.runCommand "ncmpcpp-config-wrapped" {
      buildInputs = [pkgs.makeWrapper];
      meta.priority = pkgs.ncmpcpp.meta.priority or 0;
    } ''
      makeWrapper ${pkgs.ncmpcpp}/bin/ncmpcpp $out/bin/ncmpcpp \
        --add-flags "--config=${./ncmpcpp-config}" \
        --add-flags "--bindings=${./ncmpcpp-bindings}"
    '';

  mpdConf = pkgs.writeText "mpd.conf" ''
    bind_to_address      "127.0.0.1"
    db_file             "${cfg.dataDir}/tag_cache"
    music_directory     "${cfg.musicDir}"
    playlist_directory  "${cfg.dataDir}/playlists"
    state_file          "${cfg.dataDir}/state"
    sticker_file        "${cfg.dataDir}/sticker.sql"

    audio_buffer_size "16384"
    audio_output {
      type "pulse"
      name "pulse audio"
    }

    audio_output {
      type        "fifo"
      name        "viz"
      path        "/tmp/mpd.fifo"
      format      "44100:16:2"
      buffer_time "5000" # microseconds
    }

    audio_output {
      type            "httpd"
      name            "HTTP Stream"
      encoder         "lame"
      bind_to_address "[::]"
      port            "6680"
      bitrate         "320"      # do not define if quality is defined
      format          "44100:16:1"
      always_on       "yes"      # prevent MPD from disconnecting all listeners when playback is stopped.
      tags            "yes"      # httpd supports sending tags to listening streams.
    }
  '';
in {
  options = {
    dotfiles.mpd = {
      enable = lib.mkOption {
        description = "Whether to enable mpd and related services.";
        default = false;
        type = lib.types.bool;
      };

      dataDir = lib.mkOption {
        description = ''
          The directory where MPD stores its state, tag cache,
          playlists etc.
        '';
        type = lib.types.str;
      };

      musicDir = lib.mkOption {
        description = ''
          The directory or where mpd reads music from.
        '';
        type = lib.types.str;
      };

      user = lib.mkOption {
        description = "User to run the mpd daemon as.";
        example = "ooesili";
        type = lib.types.str;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.alsa-utils
      pkgs.ncmpcpp
      ncmpcppWrapped
    ];

    systemd.user.services.mpd = {
      description = "Music Player Daemon";
      after = ["network.target" "sound.target"];
      wantedBy = ["graphical-session.target"];

      serviceConfig = {
        ExecStart = "${pkgs.mpd}/bin/mpd --no-daemon ${mpdConf}";
        LimitRTPRIO = 50;
        LimitRTTIME = "infinity";
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        Restart = "always";
      };
    };

    systemd.user.services.mpd-mpris = {
      description = "An implementation of the MPRIS protocol for MPD.";
      after = ["mpd.service"];
      wantedBy = ["mpd.service"];

      serviceConfig = {
        ExecStart = "${pkgs.mpd-mpris}/bin/mpd-mpris";
        ProtectSystem = "strict";
        Restart = "always";
      };
    };
  };
}
