{
  config,
  pkgs,
  ...
}: {
  users.users.${config.dotfiles.primaryUser}.extraGroups = ["wireshark"];

  programs.wireshark = {
    enable = true;
    usbmon.enable = true;
  };
}
