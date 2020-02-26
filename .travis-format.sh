#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# This is a standalone script so we terminate as soon as anything
# errors. See https://github.com/travis-ci/travis-ci/issues/1066
set -e
set -x

export PATH=$PATH:~/.cargo/bin

RUSTFMT_CONFIG_DIR=$DIR/rust_src

echo "Making simplified Cargo.toml"
grep -v '@CARGO' ${RUSTFMT_CONFIG_DIR}/Cargo.toml.in > ${RUSTFMT_CONFIG_DIR}/Cargo.toml

echo "Checking formatting"
cd "$DIR/rust_src"
cargo fmt -- --version

cargo fmt --all -- --config-path $RUSTFMT_CONFIG_DIR --check
