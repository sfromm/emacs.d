#!/bin/bash
# Copyright (C) 2025 by Stephen Fromm

VERSION="30.1"
BUILD_PATH="$HOME/src/emacs"
EMACS_URL="https://ftpmirror.gnu.org/emacs/emacs-${VERSION}.tar.gz"
CPU_COUNT=$(nproc --all)

7_days_ago=$(date -d 'now - 7 days' +%s)
apt_last_update=$(date -r "/var/cache/apt/pkgcache.bin" +%s)

set -e  # exit immediately if pipeline exit non-zero
set -x  # display command to stdout

mkdir -p $BUILD_PATH
cd $BUILD_PATH
[[ -e emacs-${VERSION}.tar.gz ]] || wget -c ${EMACS_URL}
[[ -d emacs-${VERSION} ]]        || tar xzf emacs-${VERSION}.tar.gz
cd emacs-${VERSION}

if (( apt_last_update <= 7_days_ago )); then
    sudo apt update
fi
sudo apt-get install -y libmagickwand-dev
sudo apt-get install -y libgccjit0 libgccjit-12-dev libjansson4 libjansson-dev libtree-sitter-dev
sudo apt-get build-dep emacs -y

./configure --prefix=/usr/local \
            --with-native-compilation=aot \
            --with-tree-sitter \
            --with-gif --with-png --with-jpeg --with-rsvg --with-tiff \
            --with-imagemagick \
            --with-pgtk \
            --with-mailutils
make clean
make -j${CPU_COUNT}
#make install
