{
  runCommand,
  bat,
  kanagawa-tm-theme,
}:
runCommand "bat-cache" {} ''
  export BAT_CACHE_PATH=$out
  mkdir themes
  cp "${kanagawa-tm-theme}/Kanagawa.tmTheme" themes
  ${bat}/bin/bat cache --build --source .
''
