export PATH="$HOME/bin:$PATH"
export ZDOTDIR="$HOME/.zsh"
export SSH_AUTH_SOCK="$HOME/.ssh/agent.ppid"
export EDITOR="vim"
export PYTHONPATH=/usr/lib/python3.3/site-packages/
# source gpg-agent environment file
[[ -f $HOME/.gnupg/agent-info ]] && source $HOME/.gnupg/agent-info
# run gpg-agent if it is not already active
if [[ -z $GPG_AGENT_INFO || $(echo $GPG_AGENT_INFO | cut -d: -f2\
        | xargs ps -o command= -c -p) != 'gpg-agent' ]]; then
    eval $(gpg-agent --daemon --write-env-file ~/.gnupg/agent-info)
fi
export GPG_AGENT_INFO

# vim: ft=sh
