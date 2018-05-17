#!/bin/bash

trap handler SIGINT

handler() {
    # Sane resume
    echo "Ctrl-c caught"
}

set -e

# Instead of make clean at top level which also recompiles all of the elisp
(cd src && make clean)
(cd rust_src && cargo clean)

# Build with full debug possibilities
CFLAGS='-O0 -g3' ./configure --enable-checking='yes,glyphs' --enable-check-lisp-object-type
make -j8  # If j8 is too aggressive for your system adjust accordingly.

# This eval ensures that escaped parameters remain escaped.
eval "src/remacs $@"
