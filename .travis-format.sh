#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# This is a standalone script so we terminate as soon as anything
# errors. See https://github.com/travis-ci/travis-ci/issues/1066
set -e
set -x

export PATH=$PATH:~/.cargo/bin

RUSTFMT_CONFIG_DIR=$DIR/rust_src

echo "Checking formatting"
cd "$DIR/rust_src"
cargo fmt -- --version

cd "$DIR/rust_src/src"
cargo fmt -- --config-path $RUSTFMT_CONFIG_DIR --write-mode=diff

cd "$DIR/rust_src/remacs-sys"
cargo fmt -- --config-path $RUSTFMT_CONFIG_DIR --write-mode=diff

cd "$DIR/rust_src/remacs-lib"
cargo fmt -- --config-path $RUSTFMT_CONFIG_DIR --write-mode=diff

cd "$DIR/rust_src/remacs-macros"
cargo fmt -- --config-path $RUSTFMT_CONFIG_DIR --write-mode=diff lib.rs

cd "$DIR/rust_src/alloc_unexecmacosx"
cargo fmt -- --config-path $RUSTFMT_CONFIG_DIR --write-mode=diff

cd "$DIR/rust_src/remacs-modules"
cargo fmt -- --config-path $RUSTFMT_CONFIG_DIR --write-mode=diff
