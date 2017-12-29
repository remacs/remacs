This directory contains Dockerfiles for various Linux distributions.

When you run one of them, be sure to include the `--security-opt seccomp=unconfined` option.
For example `docker run -it --security-opt seccomp=unconfined --name fedora-remacs fedora-latest-rust /bin/bash`. See !(../etc/PROBLEMS) for more information. If you get a failure complaining about BSS and
GAP then this is what you need to do.
