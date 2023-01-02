{runCommand}:
runCommand "keymap-us-capsctrl.map.gz" {} ''
  gzip -c ${./us-capsctrl.map} > $out
''
