{
  description = "ooesili's NixOS configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    nixos-hardware,
    rust-overlay,
  }: let
    overlays = [rust-overlay.overlays.default] ++ import ./overlays;
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      config.allowUnfree = true;
      inherit overlays;
    };

    overlayModule.nixpkgs = {
      inherit overlays;
      config.allowUnfree = true;
    };
  in {
    devShells.x86_64-linux.rustybox = pkgs.mkShell {
      buildInputs = [
        pkgs.rust-bin.stable.latest.default
      ];
    };

    overlays.default = pkgs.lib.composeManyExtensions overlays;

    templates = {
      rust = {
        description = "Rust template using the oxalica Rust overlay.";
        path = ./templates/rust;
      };
    };

    lib = {
      nixosSystem = args @ {modules, ...}:
        nixpkgs.lib.nixosSystem (args
          // {
            modules = args.modules ++ [overlayModule];
          });

      extendConfig = name: args @ {
        modules,
        system ? "x86_64-linux",
        ...
      }:
        self.lib.nixosSystem (args
          // {
            inherit system;
            modules = modules ++ [(builtins.getAttr name self.nixosModules.base)];
          });
    };

    apps.x86_64-linux.nvim = {
      type = "app";
      program = "${pkgs.neovim}/bin/nvim";
    };

    packages.x86_64-linux = {
      inherit (pkgs) neovim rustybox;
    };

    # These are turned into NixOS configurations by a private flake with some
    # additional bits I don't want to share with the world.
    nixosModules = {
      base.framework.imports = [
        nixos-hardware.nixosModules.framework-11th-gen-intel
        ./system/framework/configuration.nix
      ];
    };

    nixosConfigurations = {
      nixbox = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [overlayModule ./system/nixbox/configuration.nix];
      };

      framework = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [overlayModule ./system/framework/configuration.nix];
      };

      pinix = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [overlayModule ./system/pinix/configuration.nix];
      };

      pi-installer = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          overlayModule
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix"
          ./system/pi-installer.nix
        ];
      };

      iso = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          overlayModule
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
          ./modules/trusts.nix
        ];
      };
    };
  };
}
