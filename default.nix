{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation rec {
  version = "27.0.1";

  name = "remacs-${version}";

  buildInputs = [
    systemd texinfo libjpeg libtiff giflib xorg.libXpm gtk3
    gnutls ncurses libxml2 xorg.libXt imagemagick librsvg gpm dbus
    libotf clang_6 pkgconfig autoconf rustup
  ];

  shellHook = ''
    export NIX_PATH="nixpkgs=${toString <nixpkgs>}"
    export LIBCLANG_PATH="${llvmPackages_6.libclang.lib}/lib";
  '';
}
