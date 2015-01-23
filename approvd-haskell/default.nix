with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, blaze-html, bytestring, lens
             , lens-aeson, mtl, regex-pcre, scotty, shakespeare, stdenv, text
             , transformers, vector, wreq, hlint, reserve
             }:
             mkDerivation {
               pname = "approvd";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 aeson base blaze-html bytestring lens lens-aeson mtl regex-pcre
                 scotty shakespeare text transformers vector wreq
               ] ++ lib.optionals lib.inNixShell [ reserve hlint ];
               license = stdenv.lib.licenses.unfree;
             }) {};
in
  pkg.env
