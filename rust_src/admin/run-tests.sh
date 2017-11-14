#!/bin/sh

emacs -batch -L ./depends -l ert -l remacs-helpers.el -l remacs-helpers-tests.el -f ert-run-tests-batch-and-exit
