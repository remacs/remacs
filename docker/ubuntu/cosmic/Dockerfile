FROM ubuntu:cosmic

RUN apt-get update && apt-get dist-upgrade -y
RUN apt-get install -y \
    automake \
    build-essential \
    clang \
    curl \
    git \
    libclang-dev \
    libgif-dev \
    libgnutls28-dev \
    libgtk-3-dev \
    libjpeg-dev \
    libncurses5-dev \
    libtiff-dev \
    libxml2-dev \
    libxpm-dev \
    libxt-dev \
    texinfo


ENV PATH "/root/.cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

RUN curl https://sh.rustup.rs -o rustup.sh && \
    sh rustup.sh \
        --default-host x86_64-unknown-linux-gnu \
        --default-toolchain nightly-2019-02-27 -y && \
    rustup default nightly-2019-02-27

