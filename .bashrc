shopt -s globstar promptvars

export PS1='$(printf "%$((COLUMNS-1))s\r")\[\033[36m\][\D{%F %T}] \[\033[32m\][\u@\h \W]\n\[\033[m\]\$ '

export EDITOR=emacsclient
export PATH='/usr/local/bin:/usr/local/sbin:/Library/TeX/texbin:'$PATH
export TERM=xterm-256color

alias hnettop='nettop -d -P -j type,cell_bytes_in,cell_bytes_out,wifi_bytes_in,wifi_bytes_out,wired_bytes_in,wired_bytes_out -k rx_dupe,rx_ooo,re-tx,rtt_avg,rcvsize,tx_win,tc_class,tc_mgt,cc_algo,P,C,R,W'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"

archey --color --offline --packager

################################################################################

export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

################################################################################

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

if [ -f /usr/local/share/bash-completion/bash_completion ]; then
    . /usr/local/share/bash-completion/bash_completion
fi

################################################################################

export JAVA_HOME=$(/usr/libexec/java_home)
