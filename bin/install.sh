#!/usr/bin/env bash

pushd ~/.dotfiles

for file in $(find `pwd` -mindepth 1 -maxdepth 1 ! \( -name 'bin' -or -name '.git' \)); do
    ln -Ffhs $file ~/$(basename $file)
done

popd
