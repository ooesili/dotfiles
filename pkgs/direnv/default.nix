{ config, lib, direnv, ... }:

direnv.overrideAttrs (oldAttrs: {
  patches = [ ./use-nix-no-trace.patch ];
})
