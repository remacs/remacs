#!/bin/bash

# Add this to your Emacs config and it will format the file whenever you save.
#
#     (setq rust-format-on-save t)
#
# Then make sure this script is named `rustfmt` and in your path before ~/.cargo/bin.

LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH ~/.cargo/bin/rustfmt "$@"
