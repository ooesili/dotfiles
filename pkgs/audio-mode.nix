{
  writeShellApplication,
  ponymix,
  rofi,
}:
writeShellApplication {
  name = "audio-mode";
  runtimeInputs = [ponymix rofi];

  text = ''
    devices="$(ponymix list --sink | awk '/^sink/ {print $3}')"

    if [[ $(wc -l <<< "$devices") -eq 2 ]]; then
      default_device="$(ponymix default | awk '/^sink/ {print $3}')"
      next_device="$(printf "%s\n%s" "$devices" "$devices" | grep -A1 -m1 -Fx "$default_device" | tail -n1)"
    else
      next_device="$(rofi -dmenu -p 'audio output' -i <<< "$devices")"
    fi

    ponymix set-default --sink --device "$next_device"
  '';
}
