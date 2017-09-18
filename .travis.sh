#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# This is separate from .travis.yml so we terminate as soon as
# anything errors.
# See https://github.com/travis-ci/travis-ci/issues/1066
set -e
set -x

echo "Checking formatting"
cd "$DIR/rust_src"
cargo fmt -- --version

cd "$DIR/rust_src/src"
cargo fmt -- --write-mode=diff

cd "$DIR/rust_src/remacs-sys"
cargo fmt -- --write-mode=diff

cd "$DIR/rust_src/remacs-lib"
cargo fmt -- --write-mode=diff

cd "$DIR/rust_src/remacs-macros"
cargo fmt -- --write-mode=diff lib.rs

cd "$DIR/rust_src/alloc_unexecmacosx"
cargo fmt -- --write-mode=diff

cd "$DIR"
echo 'Configuring Emacs for building'
./autogen.sh
# These configure flags are only required on OS X.
# TODO: remove them.
./configure --without-makeinfo --with-xpm=no --with-gif=no --with-gnutls=no

echo 'Building Emacs'
make -j 3

echo 'Running C and Rust tests'
make check
