{ nixpkgs ? import <nixpkgs> {}, compiler ? "default"}:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, servant-server, stdenv, zlib }:
      mkDerivation {
        pname = "Birds";
        version = "0.1.0.0";
        sha256 = "0";
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base servant-server ];
        libraryPkgconfigDepends = [ zlib ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/colescott/birds#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
