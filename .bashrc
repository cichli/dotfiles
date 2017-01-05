shopt -s promptvars
export PS1='$(printf "%$((COLUMNS-1))s\r")\[\033[36m\][\D{%F %T}] \[\033[32m\][\u@\h \W]\n\[\033[m\]\$ '

alias ls='ls -al'

export EDITOR=emacsclient
export PATH='/usr/local/bin':$PATH
export TERM=xterm-256color

################################################################################

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

LUNCHY_DIR=$(dirname `gem which lunchy`)/../extras
if [ -f $LUNCHY_DIR/lunchy-completion.bash ]; then
    . $LUNCHY_DIR/lunchy-completion.bash
fi

################################################################################

archey --color

eval "$(ssh-agent -s)"
ssh-add -K 2>/dev/null

alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"

################################################################################

export JAVA_HOME=$(/usr/libexec/java_home)
export JAVA_TOOLS_JAR=$(/usr/libexec/java_home)'/lib/tools.jar'

# Because all Flight Recorder profiles need to live inside JAVA_HOME. Apparently.
if [ ! -f $JAVA_HOME'/jre/lib/jfr/jfr.jfc' ] && [ -f ~/.lein/jfr.jfc ]; then
    echo "Copying custom flight recorder profile to $JAVA_HOME..."
    sudo cp -f ~/.lein/jfr.jfc $JAVA_HOME'/jre/lib/jfr/'
fi

################################################################################

export SSH_USER=michael

ov() {
    cd ~/clojure/social-superstore/infrastructure/
    ENV=$1 ./deploy/overview
}

wov() {
    cd ~/clojure/social-superstore/infrastructure/
    watch ENV=$1 ./deploy/overview
}

neo() {
    cd ~/clojure/social-superstore/infrastructure/
    ./k8s/cluster/create-tunnel-all
}
