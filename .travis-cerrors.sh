#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# This is a standalone script so we terminate as soon as anything
# errors. See https://github.com/travis-ci/travis-ci/issues/1066
set -e
set -x

export PATH=$PATH:~/.cargo/bin

RUSTFMT_CONFIG_DIR=$DIR/rust_src

echo "Checking compile errors"
SKIP_BINDINGS=yes make -C "$DIR/src" generated-bindings
cd "$DIR/rust_src"
cargo build --features compile-errors 2> err.out || true
cat err.out
grep -q 'compile_error!("error 001");' err.out
