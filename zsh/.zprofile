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
# source ssh-agent environment file
[[ -f $HOME/.ssh/agent-info ]] && source $HOME/.ssh/agent-info
# run ssh-agent if it's not already active
if [[ -z $SSH_AGENT_PID ||\
        $(ps -o command= -c -p $SSH_AGENT_PID) != 'ssh-agent' ]]; then
    rm -f ~/.ssh/agent.ppid
    eval $(ssh-agent -a $SSH_AUTH_SOCK | head -2 | tee ~/.ssh/agent-info)
fi

# vim: ft=sh
