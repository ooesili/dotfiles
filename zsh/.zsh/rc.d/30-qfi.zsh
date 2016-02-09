# enable qfi directory-swiching
if which qfi &> /dev/null; then
  eval "$(qfi --shell zsh wrapper)"
fi
