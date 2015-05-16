with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, binary, bytestring, containers, filepath
             , hakyll, hjsmin, hood, process, split, stdenv, text
             }:
             mkDerivation {
               pname = "iilab-org-hakyll";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base binary bytestring containers filepath hakyll hjsmin hood
                 process split text
               ];
               license = "LGPL";
             }) {};
in
  pkg.env
