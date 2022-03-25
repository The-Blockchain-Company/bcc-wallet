# Hercules CI attribute set
# https://hercules-ci.com/github/The-Blockchain-Company/bcc-wallet
# https://docs.hercules-ci.com/hercules-ci/getting-started/minimal-repository/

let
  inherit (import ./nixpkgs-haskell.nix {}) lib;

  walletJobs = walletPkgs:
    walletPkgs.pkgs.recurseIntoAttrs {
      inherit (walletPkgs)
        bcc-wallet
        checks
        benchmarks;
    };

  nativeBuilds = builtins.mapAttrs (system: _:
    walletJobs (import ../default.nix { inherit system; })
  ) {
    x86_64-linux = {};
    # Uncomment to test build on macOS too
    # x86_64-darwin = {};
  };

  crossSystems = with lib.systems.examples;
    [ mingwW64 ];

  crossBuilds = lib.listToAttrs (map (crossSystem: {
    name = crossSystem.config;
    value = walletJobs (import ../default.nix {
      system = "x86_64-linux";
      inherit crossSystem;
    });
  }) crossSystems);

in
  nativeBuilds // crossBuilds
