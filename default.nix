{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> {} }:

with pkgs;
stdenv.mkDerivation {
  name = "sbcl";
  buildInputs = [ bash coreutils gzip sbcl rlwrap openssl gitAndTools.gitFull nix-repl ];

  LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
    pkgs.openssl
  ];
}
