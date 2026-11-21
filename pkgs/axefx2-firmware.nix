{
  stdenvNoCC,
  fetchurl,
}:
stdenvNoCC.mkDerivation {
  name = "axefx2-firmware";
  src = fetchurl {
    url = "https://launchpad.net/~albaguirre/+archive/ubuntu/axe-fx2/+sourcefiles/axefx2-usb-firmware/1.0/axefx2-usb-firmware_1.0.tar.xz";
    sha256 = "0dhawalyh9ah8snymisa1jm1l5n9wjyifp9ws0bhsypl7sv2fhy9";
  };

  phases = ["unpackPhase" "installPhase"];
  installPhase = ''
    install -Dm0755 axefx2-usb-fw.hex "$out/share/usb/axefx2.hex"
  '';
}
