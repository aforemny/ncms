rec {
  pkgs = import <nixpkgs> {};

  elm-make = pkgs.elmPackages.elm-make;
  elm-package = pkgs.elmPackages.elm-package;
  elm-repl = pkgs.elmPackages.elm-repl;

  ghc = pkgs.haskellPackages.ghc.withPackages ( hsPackages : with hsPackages;
      [ attoparsec snap directory aeson ]
  );

  ncms = pkgs.stdenv.mkDerivation {
    name = "ncms";
    buildInputs = [
      elm-make elm-package elm-repl ghc
    ];
  };
}
