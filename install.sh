#!/usr/bin/env bash

pushd ~/.dotfiles

for file in $(find `pwd` -mindepth 1 -maxdepth 1 ! \( -name '.git' -or -name '.gitignore' -or -name 'install.sh' -or -name 'update.sh' \)); do
    ln -Ffhs $file ~/$(basename $file)
done

popd
