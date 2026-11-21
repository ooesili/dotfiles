{pkgs, ...}: let
  productID = "0003";
  vendorID = "2466";
in {
  config = {
    services.udev.extraRules = ''
      # Fractal Audio Systems Axe-FX II
      ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="${vendorID}", ATTR{idProduct}=="${productID}", RUN+="${pkgs.fxload}/bin/fxload -t fx2 -d ${vendorID}:${productID} -i ${pkgs.axefx2-firmware}/share/usb/axefx2.hex"

      # realtime audio
      KERNEL=="rtc0", GROUP="audio"
      KERNEL=="hpet", GROUP="audio"
    '';
  };
}
