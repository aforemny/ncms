rec {
  pkgs = import <nixpkgs> {};

  elm-make = pkgs.elmPackages.elm-make;
  elm-package = pkgs.elmPackages.elm-package;
  elm-repl = pkgs.elmPackages.elm-repl;

  ghc = pkgs.haskellPackages.ghc.withPackages ( hsPackages : with hsPackages;
    [ aeson attoparsec base bytestring containers directory filepath mtl
      process snap snap-server text unordered-containers vector
    ]
  );

  ncms =
    pkgs.haskellPackages.callPackage ./ncms.nix {};

  env = pkgs.stdenv.mkDerivation {
    name = "ncms-env";
    buildInputs = [ ghc elm-make elm-package elm-repl ];
  };
}
