with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, containers, filepath, hakyll
             , hjsmin, hood, split, stdenv, text
             }:
             mkDerivation {
               pname = "iilab-org-hakyll";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base bytestring containers filepath hakyll hjsmin hood split text
               ];
               license = "LGPL";
             }) {};
in
  pkg.env
