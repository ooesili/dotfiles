ROLE := $(shell cat ./active-role)

.PHONY: default
default:

.PHONY: build
build: role
	nix-build user/${ROLE}.nix

.PHONY: user role
user:
	mkdir -p ~/.nixpkgs
	cp user/nixpkgs-config.nix ~/.nixpkgs/config.nix
	nix-env -rif user/${ROLE}.nix

.PHONY: system
system: role system/local.nix
	install -m0644 system/local.nix /etc/nixos/local.nix
	install -m0644 system/${ROLE}.nix /etc/nixos/configuration.nix
	install -m0644 system/base.nix /etc/nixos/base.nix
	install -m0644 system/libreswan.nix /etc/nixos/libreswan.nix
	rm -rf /etc/nixos/modules
	cp -R modules /etc/nixos/
	nixos-rebuild switch

.PHONY: clean
clean:
	rm result*

.PHONY: role
role:
ifndef ROLE
	$(error $$ROLE must be set)
endif
