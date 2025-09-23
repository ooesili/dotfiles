# environment variables
set -gx EDITOR nvim
set -gx MANWIDTH 78
set -gx FZF_DEFAULT_COMMAND 'rg --files --hidden --follow --glob "!.git/*" 2> /dev/null'
set -gx FZF_DEFAULT_OPTS '--exact'
set -gx MANPAGER 'nvim +Man!'

set -gx LESS_TERMCAP_mb "[0;31m"  # begin blinking mode
set -gx LESS_TERMCAP_md "[0;32m"  # begin bold mode [headers]
set -gx LESS_TERMCAP_me "[0m"     # end blink/bold mode
set -gx LESS_TERMCAP_us "[0;33m"  # begin underline [variables]
set -gx LESS_TERMCAP_ue "[0m"     # end underline
set -gx LESS_TERMCAP_so "[0;34m"  # begin standout [info box]
set -gx LESS_TERMCAP_se "[0m"     # end standout

fish_add_path ~/.local/bin ~/bin ~/.cargo/bin ~/.local/share/npm-packages/bin

if ! status is-interactive
  return
end

# disable greeting
set -U fish_greeting

# plugins
starship init fish | source
zoxide init fish | source
direnv hook fish | source
fzf --fish | source

# zoxide customization
function z --wraps=__zoxide_z --description 'alias z=__zoxide_z'
  if test (count $argv) -eq 0
    zi
  else
    __zoxide_z $argv
  end
end

function mkcd
  if test (count $argv) -eq 0
    echo 'usage: mkcd <new-directory>' >&2
    return 1
  end
  set -l dir $argv[1]

  if test -d $dir
    echo "`$dir' already exists: cd-ing."
  else
    mkdir -p $dir
  end
  cd $dir
end

function gclone
  set -l repo $argv[1]
  set -l full_path ~/src/github.com/$repo
  set -l url git@github.com:$repo.git

  if test -d $full_path
    echo "directory already exists; cd-ing into $full_path"
    cd $full_path
    return
  end

  echo "cloning from GitHub: $url"
  if git clone $url $full_path
    cd $full_path
  else
    return 1
  end
end

# don't try to use nix's default impl, as it requires channels
function fish_command_not_found
  __fish_default_command_not_found_handler $argv
end

function cdt
  set -l dir (mktemp -d)
  echo $dir
  cd $dir
end

function fork
  command $argv &> /dev/null &;
  if test -n $last_pid
    disown $last_pid &> /dev/null
  end
end

function sysnix
  if test (count $argv) -lt 1
    command nix
  else
    nix $argv[1] --inputs-from ~/sync/dotfiles/nix-config $argv[2..]
  end
end

function nix-source
  set -l argc (count $argv)
  set -l input

  if test $argc -eq 0
    set input nixpkgs
  else if test $argc -eq 1
    set input $argv[1]
  else
    echo 'error: too many arguments'
    echo 'usage: nix-source <input>'
    return 1
  end

  cd (nix flake metadata --inputs-from ~/sync/dotfiles/nix-config $input --json | jq -r '.path')
end

function snr
  sysnix run nixpkgs#$argv[1] $argv[2..]
end

function sns
  sysnix shell nixpkgs#$argv[1] $argv[2..]
end

function sne
  sysnix edit nixpkgs#$argv[1] $argv[2..]
end

# abbreviations
alias ls=eza
alias rg="rg --type-add 'tf:*.tf' --type-add 'tfvars:*.tfvars'"
alias screenshot='grim -g (slurp)'
alias ta='tmux attach'
alias ts='tmux new -s'
alias tx='tmux resize-pane -x'
alias ty='tmux resize-pane -y'
abbr -a de direnv edit
abbr -a dr direnv reload
abbr -a l ls -l
abbr -a la ls -la
abbr -a mine sudo chown -R (id -u):(id -g)

# git
function git_main_branch
  if ! command git rev-parse --git-dir &> /dev/null
    return 1
  end

  set refs refs/{heads,remotes/{origin,upstream}}/{main,trunk,mainline,default,stable,master}
  for ref in $refs
    if command git show-ref -q --verify $ref
      basename $ref
      return
    end
  end

  # no main branch found
  echo 'no main branch found' >&2
  return 1
end
alias ga='git add'
alias gaa='git add --all'
alias gapa='git add --patch'
alias gb='git branch'
alias gbd='git branch --delete'
alias gbm='git branch --move'
alias gc='git commit --verbose'
alias gcm='git checkout (git_main_branch)'
alias gcn!='git commit --verbose --no-edit --amend'
alias gco='git checkout'
alias gd='git diff'
alias gdca='git diff --cached'
alias gl='git pull'
alias glo='git log --oneline --decorate'
alias gp='git push'
alias gpoh='git push -u origin HEAD'
alias gpoh='git push -u origin HEAD'
alias grb='git rebase'
alias grgm='git rebase (git_main_branch)'
alias gru='git add --patch'
alias gst='git status'
function git-review
  if test -n "$(git status --porcelain)"
    echo 'warning: skipping due to uncomitted changes' >&2
    return 1
  end

  set main (git_main_branch)
  set rev (git rev-list -n 1 $main)
  git reset --soft $rev
  git restore --staged .
  git add -N .
end

function tempshell
  TEMPSHELL=1 _ZO_EXCLUDE_DIRS="*" fish -P
end
