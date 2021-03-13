{ pkgs }:
let
  lib = pkgs.lib;

  util = import ./util.nix {
    inherit pkgs;
    inherit (pkgs) lib gitignoreFilter;
  };

  conf = lib.importTOML ../nixkell.toml;

  # Create our haskell from the choosen version of the default one
  ourHaskell = pkgs.haskell.packages.${("ghc" + util.removeDot conf.ghc)}.override {
    overrides =
      let
        depsFromDir = pkgs.haskell.lib.packagesFromDirectory {
          directory = ./packages;
        };
        manual = _hfinal: hprev: {
          ory-kratos =
            let
              filteredSrc = util.filterSrc ../. {
                ignoreFiles = conf.ignore.files;
                ignorePaths = conf.ignore.paths;
              };
            in
            hprev.callCabal2nix "ory-kratos" filteredSrc { };
        };
      in
      lib.composeExtensions depsFromDir manual;
  };

  # Include our package dependencies with ghc
  ghc = ourHaskell.ghc.withPackages (_ps:
    pkgs.haskell.lib.getHaskellBuildInputs ourHaskell.ory-kratos
  );

  tools = util.buildWith ourHaskell [ "haskell-language-server" ] conf.env.tools;

  scripts = import ./scripts.nix { inherit pkgs; };
in
{
  bin = util.leanPkg ourHaskell.ory-kratos;

  shell = pkgs.buildEnv {
    name = "ory-kratos-env";
    paths = [ ghc ] ++ tools ++ scripts;
  };
}
