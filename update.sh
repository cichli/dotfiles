#!/usr/bin/env bash

set -o verbose
set -e

##############
## homebrew ##
##############
brew update
brew bundle dump --global --force
brew bundle cleanup --global --force
brew bundle install --global --verbose --no-upgrade
brew upgrade --fetch-HEAD

###########
## emacs ##
###########
curl -I https://elpa.gnu.org/
curl -I https://melpa.org/
curl -I http://orgmode.org/elpa/

pushd ~/elisp/cider
git fetch --prune --all
git rebase --autostash upstream/master
rm -rf cider-pkg.el
gmake elpaclean elpa autoloads
popd

pushd ~/elisp/clojure-mode
git fetch --prune --all
git rebase --autostash upstream/master
gmake clean elpa
cask exec emacs --quick --batch \
     --eval "(progn
               (require 'package)
               (package-generate-autoloads 'clojure-mode default-directory))"
popd

pushd ~/.emacs.d
cask link cider ~/elisp/cider
cask link clojure-mode ~/elisp/clojure-mode
cask
cask update
popd
