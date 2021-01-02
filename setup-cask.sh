#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset

EMACS_MAJOR_VERSION="$(echo "$EMACS_VERSION" | cut -f 1 -d '.')"
if [ "$EMACS_MAJOR_VERSION" = "27" ];then
    cp Cask-27 Cask
elif [ "$EMACS_MAJOR_VERSION" = "26" ];then
    cp Cask-26 Cask
else 
    cp Cask-default Cask
fi
