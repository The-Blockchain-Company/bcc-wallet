{ system ? builtins.currentSystem
, crossSystem ? null
# Lets you customise ghc and profiling (see ./haskell.nix):
, config ? {}
# Lets you override niv dependencies of the project without
# modifications to the source.
# eg. to test build against a local checkout of bcc-node:
#   nix-build default.nix -A bcc-wallet --arg sourcesOverride '{ nixpkgs = ../nixpkgs; }'
, sourcesOverride ? {}
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  tbcoNixMain = import sources.tbco-nix {};
  haskellNix = import sources."haskell.nix" {
    inherit system;
    # if niv sources hackage or stackage are present, pass them
    # through to Haskell.nix.
    sourcesOverride =
      (if builtins.hasAttr "hackage" sources then { inherit (sources) hackage; } else {}) //
      (if builtins.hasAttr "stackage" sources then { inherit (sources) stackage; } else {})
      // sourcesOverride;
  };
  # use our own nixpkgs if it exists in our sources,
  # otherwise use tbcoNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then
      (builtins.trace "Not using Haskell.nix nixpkgs pin (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    # else tbcoNixMain.nixpkgs;
    else haskellNix.sources.nixpkgs-unstable;

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/The-Blockchain-Company/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ tbcoNixMain.overlays.haskell-nix-extra
    # tbcoNix: tbcoNix.lib and niv packages:
    ++ tbcoNixMain.overlays.tbcoNix
    # tbcoNix: utils
    ++ tbcoNixMain.overlays.utils
    # tbcoNix: crypto
    ++ tbcoNixMain.overlays.crypto
    # our own overlays:
    ++ [
      (final: prev: {

        # commonLib: tbco-nix utils and our own:
        commonLib = final.tbcoNix
          // import ./util.nix { inherit (final) lib; }
          # also expose our sources and overlays
          // { inherit overlays sources; };
      })
      # Haskell build tools
      (import ./overlays/build-tools.nix)
      # Bcc deployments
      (import ./overlays/bcc-deployments.nix)
      # Other packages overlay
      (import ./overlays/pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

in pkgs
