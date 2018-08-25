.PHONY: default
default:

.PHONY: all
all: sys user

.PHONY: build
build:
	nix-build user

.PHONY: user
user:
	nix-env -rif user

.PHONY: sys
sys:
	install -m0644 system/configuration.nix /etc/nixos/configuration.nix
	nixos-rebuild switch

.PHONY: clean
clean:
	rm result*
