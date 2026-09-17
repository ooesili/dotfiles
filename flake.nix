{
  description = "ooesili's NixOS configurations";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixpkgs-unstable/nixexprs.tar.zst";
    mnw.url = "github:Gerg-L/mnw";

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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
    mnw,
  }: let
    overlays = [
      rust-overlay.overlays.default
      (import ./overlays {inherit mnw;})
    ];
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
        nixpkgs.lib.nixosSystem (
          args
          // {
            modules = args.modules ++ [overlayModule];
          }
        );

      extendConfig = name: args @ {
        modules,
        system ? "x86_64-linux",
        ...
      }:
        self.lib.nixosSystem (
          args
          // {
            inherit system;
            modules = modules ++ [(builtins.getAttr name self.nixosModules.base)];
          }
        );
    };

    packages.x86_64-linux = {
      inherit (pkgs) neovim rustybox direnv;

      kanagawa = pkgs.callPackage ./pkgs/kanagawa {};
      alacritty = pkgs.callPackage ./pkgs/alacritty-config {
        config.fontSize = "9.5";
      };
    };

    # These are turned into NixOS configurations by a private flake with some
    # additional bits I don't want to share with the world.
    nixosModules = {
      base.framework.imports = [
        nixos-hardware.nixosModules.framework-11th-gen-intel
        ./system/framework/configuration.nix
      ];

      shell = ./modules/shell.nix;
    };

    nixosConfigurations = {
      nixbox = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          overlayModule
          ./system/nixbox/configuration.nix
        ];
      };

      framework = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          overlayModule
          ./system/framework/configuration.nix
        ];
      };

      pinix = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          overlayModule
          ./system/pinix/configuration.nix
        ];
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
