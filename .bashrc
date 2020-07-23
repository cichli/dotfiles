export PATH='/usr/local/bin:/usr/local/sbin:/Library/TeX/texbin:'$PATH

export EDITOR=emacsclient
export LESS='--ignore-case --LONG-PROMPT --RAW-CONTROL-CHARS --status-column --window=-2'
export LESSOPEN='| lesspipe.sh %s' LESS_ADVANCED_PREPROCESSOR=1
export PAGER=less

tic -x -o ~/.terminfo ~/.terminfo/xterm-24bit.terminfo
export TERM=xterm-24bit

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
jenv macos-javahome

export LEIN_USE_BOOTCLASSPATH=no

export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

alias ssh='TERM=xterm-256color ssh'

alias hnettop='nettop -d -P -j type,cell_bytes_in,cell_bytes_out,wifi_bytes_in,wifi_bytes_out,wired_bytes_in,wired_bytes_out -k rx_dupe,rx_ooo,re-tx,rtt_avg,rcvsize,tx_win,tc_class,tc_mgt,cc_algo,P,C,R,W'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias tb='nc termbin.com 9999'

[[ $- = *i* ]] && [[ -r /usr/local/share/liquidprompt ]] && source /usr/local/share/liquidprompt

export BASH_COMPLETION_COMPAT_DIR=/usr/local/etc/bash_completion.d
[[ -r /usr/local/etc/profile.d/bash_completion.sh ]] && source /usr/local/etc/profile.d/bash_completion.sh

[[ -r ~/.iterm2_shell_integration.bash ]] && source ~/.iterm2_shell_integration.bash

archey --color --offline --packager

# Disable Ctrl+Cmd+D keyboard shortcut
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
