{ config, pkgs, lib, ... }:

let
  cfg = config.dotfiles.mpd;

  ncmpcppWrapped = with pkgs; runCommand "ncmpcpp-config-wrapped" {
    buildInputs = [ makeWrapper ];
    meta.priority = ncmpcpp.meta.priority or 0;
  } ''
    makeWrapper ${ncmpcpp}/bin/ncmpcpp $out/bin/ncmpcpp \
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
      ${if config.hardware.pulseaudio.enable
        then ''
          type "pulse"
          name "pulse audio"
        ''
        else ''
          type   "alsa"
          name   "Default Output"
          device "default:CARD=${cfg.soundCard}"
        ''
      }
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

in with lib; {
  options = {
    dotfiles.mpd = {
      dataDir = mkOption {
        description = ''
          The directory where MPD stores its state, tag cache,
          playlists etc.
        '';
        type = types.str;
      };

      musicDir = mkOption {
        description = ''
          The directory or where mpd reads music from.
        '';
        type = types.str;
      };

      soundCard = mkOption {
        description = "Primary sound card name (from /proc/asound/cards).";
        example = "PCH";
        type = types.str;
      };

      user = mkOption {
        description = "User to run the mpd daemon as.";
        example = "ooesili";
        type = types.str;
      };
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      alsaUtils
      ncmpcpp
      ncmpcppWrapped
    ];

    systemd.user.services.mpd = {
      description = "Music Player Daemon";
      after = [ "network.target" "sound.target" ];
      wantedBy = [ "graphical-session.target" ];

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

    sound = {
      enable = true;
      extraConfig = ''
        defaults.pcm.!card ${cfg.soundCard}
      '';
    };
  };
}
