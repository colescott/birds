{ pkgs ? import <nixpkgs> {} }: with pkgs;

stdenv.mkDerivation rec {
    name = "birds-env";
    version = "0.1";
    buildInputs = with pkgs; [
        flow
        nodejs-7_x
        yarn
        elmPackages.elm
        elmPackages.elm-format
        mongodb
    ];
    src = ./.;
}
