.PHONY: *

default:

user:
	mkdir -p ~/.nixpkgs
	cp user/nixpkgs-config.nix ~/.nixpkgs/config.nix
	nix-env --install --remove-all --file user/$(shell cat ./active-role).nix

system:
	nixos-rebuild switch --flake ".#$(cat /etc/hostname)"

clean:
	rm result*

iso:
	nix build '.#nixosConfigurations.iso.config.system.build.isoImage'
