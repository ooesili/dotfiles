export GOPATH="$HOME/golang"
export PATH="$HOME/bin:$GOPATH/bin:$PATH"

export ZDOTDIR="$HOME/.zsh"
export SSH_AUTH_SOCK="$HOME/.ssh/agent.ppid"
export EDITOR="vim"
export PYTHONPATH=/usr/lib/python3.3/site-packages/

# XDG Base Directory Specification
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

# source ssh-agent environment file
[[ -f $HOME/.ssh/agent-info ]] && source $HOME/.ssh/agent-info
# run ssh-agent if it's not already active
if [[ -z $SSH_AGENT_PID ||\
        $(ps -o command= -c -p $SSH_AGENT_PID) != 'ssh-agent' ]]; then
    rm -f ~/.ssh/agent.ppid
    eval $(ssh-agent -a $SSH_AUTH_SOCK | head -2 | tee ~/.ssh/agent-info)
fi

# vim: ft=sh
