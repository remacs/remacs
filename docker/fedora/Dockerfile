FROM fedora:latest

RUN yum install -y \
    git \
    gcc gcc-c++ \
    kernel-devel \
    make \
    automake \
    ncurses-devel \
    giflib-devel \
    libjpeg-turbo-devel \
    libtiff-devel \
    gtk3-devel \
    libxml2-devel \
    libXpm-devel \
    gnutls-devel \
    curl \
    texinfo

ENV PATH "/root/.cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

RUN curl https://sh.rustup.rs -o rustup.sh && \
    sh rustup.sh --default-toolchain nightly-2017-12-23 -y && \
    rustup default nightly-2017-12-23
