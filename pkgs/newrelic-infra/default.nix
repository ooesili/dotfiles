{
  stdenv,
  buildGo118Module,
  fetchFromGitHub,
  lib,
}: let
  version = "1.20.7";
in
  buildGo118Module {
    inherit version;
    pname = "newrelic-infra-agent";

    src = fetchFromGitHub {
      owner = "newrelic";
      repo = "infrastructure-agent";
      rev = version;
      sha256 = "sha256-bC0GOybk5r6jwV44i5Wc+7M468AYSJpysma0yv8OdPo=";
    };

    patches = [./path-fix.patch];

    subPackages = [
      "cmd/newrelic-infra"
      "cmd/newrelic-infra-ctl"
      "cmd/newrelic-infra-service"
    ];

    vendorSha256 = "sha256-3bXIb58etrnyFah3WK4hBuyVk6lxI8I5Oblv8X0ANfg=";

    meta = {
      homepage = "https://github.com/newrelic/infrastructure-agent";
      description = " New Relic Infrastructure Agent";
      license = lib.licenses.asl20;
    };
  }
