NixOS Dotfiles
==============

This repository contains my NixOS and Nix configurations.

## TODO

Make i3 config symbolic and hot swapable

## Install Process

```
$ sudo nix-channel --add  https://nixos.org/channels/nixos-unstable unstable
$ sudo nix-channel --update
```

Partitions and filesystems

Decrypt and mount volume with nix configs

```
$ sudo nixos-generate-config --root /mnt
```
Targeted install of nixos config files to /mnt/etc/nixos

```
$ sudo nixos-install -j 7
```

Reset root password

Set ooesili password

Extract wireguard and luks keys

```
$ sudo mkdir -p /media/exthd
```

```
$ touch ~/.zshrc
$ touch ~/.z
```

create restic key for cron job

create up to date syncthing bin package

add binary terraform repo or fetcher script

## Buliding a Raspberry Pi Installation Image

```
$ nix build .#nixosConfigurations.pi-installer.config.system.build.sdImage
```
