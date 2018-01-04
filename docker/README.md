# Docker support for Remacs

This directory contains Dockerfiles for various Linux
distributions. They are generated from a template in the top of this
directory.

## Docker Usage

When you run one of them, be sure to include the `--security-opt
seccomp=unconfined` option.  For example `docker run -it
--security-opt seccomp=unconfined --name fedora-remacs
fedora-latest-rust /bin/bash`. See !(../etc/PROBLEMS) for more
information. If you get a failure complaining about BSS and GAP then
this is what you need to do.

## About the templates

This is a really simple setup inspired by Ansible but it has no
external depends. Each directory represents another Docker
container. The `config.yaml` file is read and all of the fields become
a dictionary in the Python code and are variables in the template. The
`rust-toolchain` file is read as well to pass the version down into
the generated files.

## Generating Dockerfiles

Just run `./generate` in the `docker` directory.
