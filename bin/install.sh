#!/usr/bin/env bash

pushd ~/.dotfiles

for file in $(find `pwd` -mindepth 1 -maxdepth 1 ! \( -name 'bin' -or -name '.git' -or -name '.gitignore' \)); do
    ln -Ffhs $file ~/$(basename $file)
done

popd
