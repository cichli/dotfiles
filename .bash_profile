shopt -s promptvars
export PS1='$(printf "%$((COLUMNS-1))s\r")\[\033[36m\][\D{%F %T}] \[\033[32m\][\u@\h \W]\n\[\033[m\]\$ '

alias ls='ls -al'

export EDITOR=emacsclient
export PATH='/usr/local/bin':$PATH
export TERM=xterm-256color

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

LUNCHY_DIR=$(dirname `gem which lunchy`)/../extras
if [ -f $LUNCHY_DIR/lunchy-completion.bash ]; then
    . $LUNCHY_DIR/lunchy-completion.bash
fi

archey -c

eval "$(ssh-agent -s)"

################################################################################

export JAVA_HOME=$(/usr/libexec/java_home)
export JAVA_TOOLS_JAR=$(/usr/libexec/java_home)'/lib/tools.jar'

# Because all Flight Recorder profiles need to live inside JAVA_HOME. Apparently.
if [ -f ~/.lein/jfr.jfc ]; then
    cp -f ~/.lein/jfr.jfc $JAVA_HOME'/jre/lib/jfr/'
fi
