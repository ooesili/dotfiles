() {
  local funcdir="$ZDOTDIR/functions"
  fpath=($funcdir $fpath)
  autoload $(/usr/bin/ls $funcdir)
}
