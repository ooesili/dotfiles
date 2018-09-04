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
	install -m0644 system/configuration.nix /etc/nixos/configuration.nix
	nixos-rebuild switch

.PHONY: clean
clean:
	rm result*
