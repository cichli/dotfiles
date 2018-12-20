shopt -s globstar promptvars

export PS1='$(printf "%$((COLUMNS-1))s\r")\[\033[36m\][\D{%F %T}] \[\033[32m\][\u@\h \W]\n\[\033[m\]\$ '

export EDITOR=emacsclient
export PATH='/usr/local/bin:/usr/local/sbin:/Library/TeX/texbin:'$PATH
export TERM=xterm-256color

alias hnettop='nettop -d -P -j type,cell_bytes_in,cell_bytes_out,wifi_bytes_in,wifi_bytes_out,wired_bytes_in,wired_bytes_out -k rx_dupe,rx_ooo,re-tx,rtt_avg,rcvsize,tx_win,tc_class,tc_mgt,cc_algo,P,C,R,W'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"

archey --color --offline --packager

eval `keychain --eval --agents ssh --inherit any id_rsa`

################################################################################

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

if [ -f /usr/local/share/bash-completion/bash_completion ]; then
    . /usr/local/share/bash-completion/bash_completion
fi

################################################################################

export JAVA_HOME=$(/usr/libexec/java_home)
export JAVA_TOOLS_JAR=$(/usr/libexec/java_home)'/lib/tools.jar'

# Because all Flight Recorder profiles need to live inside JAVA_HOME. Apparently.
if [ ! -f $JAVA_HOME'/lib/jfr/jfr.jfc' ] && [ -f ~/.lein/jfr.jfc ]; then
    echo "Copying custom flight recorder profile to $JAVA_HOME..."
    sudo cp -f ~/.lein/jfr.jfc $JAVA_HOME'/lib/jfr/'
fi
