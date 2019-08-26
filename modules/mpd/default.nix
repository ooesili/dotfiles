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

    services.mpd = {
      dataDir = cfg.dataDir;
      enable = true;
      group = "users";
      musicDirectory = cfg.musicDir;
      user = cfg.user;

      extraConfig = if config.hardware.pulseaudio.enable then
        ''
          audio_output {
            type "pulse"
            name "pulse audio"
          }
        ''
      else
        ''
          audio_output {
            type   "alsa"
            name   "Default Output"
            device "default:CARD=${cfg.soundCard}"
          }
        '';
    };

    sound = {
      enable = true;
      extraConfig = ''
        defaults.pcm.!card ${cfg.soundCard}
      '';
    };
  };
}
