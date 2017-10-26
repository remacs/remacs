#!/bin/bash

LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH ~/.cargo/bin/rustfmt "$@"
