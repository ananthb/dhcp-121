{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "uts-qr";

  buildInputs = with pkgs; [
    elmPackages.elm
  ];
}
