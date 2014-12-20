#!/bin/bash

for file in $(find `pwd` -mindepth 1 -maxdepth 1 ! \( -name '.git' -or -name '.gitignore' -or -name 'install.sh' \)); do
    ln -Ffhs $file ~/$(basename $file)
done
