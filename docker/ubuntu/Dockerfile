FROM ubuntu:xenial

RUN apt-get update && apt-get install -y \
    automake \
    build-essential \
    curl \
    libgif-dev \
    libgnutls-dev \
    libgtk-3-dev \
    libjpeg-dev \
    libncurses5-dev \
    libtiff-dev \
    libxml2-dev \
    libxpm-dev \
    texinfo


ENV PATH "/root/.cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

RUN curl https://sh.rustup.rs -o rustup.sh && \
    sh rustup.sh --default-host x86_64-unknown-linux-gnu --default-toolchain nightly-2017-12-23 -y && \
    rustup default nightly-2017-12-23

