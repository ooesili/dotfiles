{
  config,
  lib,
  pkgs,
  options,
  ...
}: let
  trustedUserCAKeys = pkgs.writeText "ssh-ca.pub" ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICOtcKUm3wJC4eFXIImiG7ElxuW7zF3DTj2EmZ8i9YuB certificate-authority
  '';
in {
  nix.settings.trusted-public-keys = [
    "192.168.101.2:D1DpppsXiUjx6rLygZ9D2kleWUI+vNlinPvLEUqydeQ="
  ];

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    extraConfig = ''
      TrustedUserCAKeys ${trustedUserCAKeys}
    '';
  };
}
