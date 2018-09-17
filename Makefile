ROLE := $(shell cat /etc/nixos-role)

.PHONY: default
default:

.PHONY: build
build:
	nix-build user

.PHONY: user
user:
	nix-env -rif user

.PHONY: system
system:
	install -m0644 system/${ROLE}.nix /etc/nixos/configuration.nix
	install -m0644 system/base.nix /etc/nixos/base.nix
	nixos-rebuild switch

.PHONY: clean
clean:
	rm result*
