{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> {} }:

with pkgs;
stdenv.mkDerivation {
  name = "sbcl";
  buildInputs = [ bash coreutils sbcl rlwrap openssl ];

  LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
    pkgs.openssl
  ];
}
