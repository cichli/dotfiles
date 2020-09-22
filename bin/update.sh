#!/usr/bin/env bash

set -o verbose
set -e

brew update
brew bundle dump --global --force
brew bundle cleanup --global --force
brew bundle install --global --verbose --no-upgrade
brew upgrade --fetch-HEAD
mas upgrade
