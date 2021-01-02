#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset

EMACS_VERSION="$(emacs --version | head -n 1 | cut -d ' ' -f 3)"

if [ "$EMACS_VERSION" = "27" ];then
    cp Cask-27.1 Cask
else
    cp Cask-default Cask
fi
