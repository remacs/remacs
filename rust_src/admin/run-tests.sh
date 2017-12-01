#!/bin/sh

../../src/remacs -batch \
                 -L ./depends \
                 -l ert -l remacs-helpers.el -l remacs-helpers-tests.el \
                 --eval "(ert-run-tests-batch-and-exit test-order)"
