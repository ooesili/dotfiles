ROLE := $(shell cat ./active-role)

.PHONY: default
default:

.PHONY: build
build: role
	nix-build user/${ROLE}.nix

.PHONY: user role
user:
	install -Dm0644 user/nixpkgs-config.nix ~/.nixpkgs/config.nix
	nix-env -rif user/${ROLE}.nix

.PHONY: system
system: role system/local.nix
	install -m0644 system/local.nix /etc/nixos/local.nix
	install -m0644 system/${ROLE}.nix /etc/nixos/configuration.nix
	install -m0644 system/base.nix /etc/nixos/base.nix
	nixos-rebuild switch

.PHONY: clean
clean:
	rm result*

.PHONY: role
role:
ifndef ROLE
	$(error $$ROLE must be set)
endif
