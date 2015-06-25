alias ls='ls -al'

export JAVA_HOME=$(/usr/libexec/java_home)
export JAVA_TOOLS_JAR='/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home/lib/tools.jar'
export PATH='/usr/local/bin':'~/.cask/bin':$PATH
export TERM=xterm-256color

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
  fi

archey -c

eval "$(ssh-agent -s)"
