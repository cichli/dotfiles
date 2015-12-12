alias ls='ls -al'

export EDITOR=emacsclient

export JAVA_HOME=$(/usr/libexec/java_home)
export JAVA_TOOLS_JAR='/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/lib/tools.jar'

export PATH='/usr/local/bin':$PATH
export TERM=xterm-256color

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
  fi

archey -c

eval "$(ssh-agent -s)"
