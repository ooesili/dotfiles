{
  buildGo126Module,
  fetchFromGitHub,
  lib,
}: let
  version = "1.43.1";
in
  buildGo126Module {
    inherit version;
    pname = "newrelic-infra-agent";

    src = fetchFromGitHub {
      owner = "newrelic";
      repo = "infrastructure-agent";
      rev = version;
      sha256 = "sha256-DC0RIMkpX80o6shDyhZcZ5novPSPNqgi4FFIbRmh/Yk=";
    };

    patches = [./path-fix.patch];
    doCheck = false;

    subPackages = [
      "cmd/newrelic-infra"
      "cmd/newrelic-infra-ctl"
      "cmd/newrelic-infra-service"
    ];

    vendorHash = "sha256-izjfwwKHR0tSuO+bjU5NT8r+uu8EhWl20GIfMjytNHk=";

    meta = {
      homepage = "https://github.com/newrelic/infrastructure-agent";
      description = " New Relic Infrastructure Agent";
      license = lib.licenses.asl20;
    };
  }
