HOST := $(shell cat /etc/hostname)

.PHONY: *

build:
	nixos-rebuild build --flake .#${HOST} --print-build-logs

switch:
	sudo nixos-rebuild switch --flake .#${HOST}

boot:
	sudo nixos-rebuild boot --flake .#${HOST}

test:
	sudo nixos-rebuild test --flake .#${HOST}

clean:
	rm result*

iso:
	nix build '.#nixosConfigurations.iso.config.system.build.isoImage'
