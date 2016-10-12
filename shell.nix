with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, Cabal, conduit
             , conduit-combinators, containers, directory, filepath
             , mono-traversable, process, regex-tdfa, shell-conduit, stdenv
             , template-haskell, text, transformers
             }:
             mkDerivation {
               pname = "checkcert";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base bytestring Cabal conduit conduit-combinators containers
                 directory filepath mono-traversable process regex-tdfa
                 shell-conduit template-haskell text transformers
               ];
               license = stdenv.lib.licenses.agpl3;
             }) {};
in
  pkg.env
