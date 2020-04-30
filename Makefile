ROLE := $(shell cat ./active-role)

.PHONY: *

default:

user:
	mkdir -p ~/.nixpkgs
	cp user/nixpkgs-config.nix ~/.nixpkgs/config.nix
	nix-env -rif user/${ROLE}.nix

system: install
	nixos-rebuild switch

install: role
	install -m0644 system/local.nix /etc/nixos/local.nix
	install -m0644 system/${ROLE}.nix /etc/nixos/configuration.nix
	install -m0644 system/base.nix /etc/nixos/base.nix
	rm -rf /etc/nixos/{modules,pkgs}
	cp -R {modules,pkgs} /etc/nixos/

clean:
	rm result*

role:
ifndef ROLE
	$(error $$ROLE must be set)
endif
