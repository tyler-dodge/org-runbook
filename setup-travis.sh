#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset
EMACS_MAJOR_VERSION="$(echo $EMACS_VERSION | cut -d '.' -f 1)"

if [ $EMACS_MAJOR_VERSION = 27 -o $EMACS_MAJOR_VERSION = 26 ]; then
fi
make -f emacs-travis.mk install_emacs
make -f emacs-travis.mk install_cask
