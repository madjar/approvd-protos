with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, bytestring, lens, lens-aeson, mtl
             , regex-pcre, stdenv, text, transformers, wreq, reserve, hlint
             }:
             mkDerivation {
               pname = "approvd";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 aeson base bytestring lens lens-aeson mtl regex-pcre text
                 transformers wreq
               ] ++ lib.optionals lib.inNixShell [ reserve hlint ];
               license = stdenv.lib.licenses.unfree;
             }) {};
in
  pkg.env
