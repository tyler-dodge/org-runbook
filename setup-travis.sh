#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset
EMACS_MAJOR_VERSION="$(echo $EMACS_VERSION | cut -d '.' -f 1)"
if  [ -d "$HOME/emacs" ]; then
    emacs --version
    echo emacs already installed.
    exit
fi
if [ $EMACS_MAJOR_VERSION = 27 -o $EMACS_MAJOR_VERSION = 26 ]; then
    make -f emacs-travis.mk install_gnutls
fi
make -f emacs-travis.mk install_emacs
make -f emacs-travis.mk install_cask
