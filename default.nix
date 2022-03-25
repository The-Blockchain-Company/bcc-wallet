############################################################################
#
# Bcc Wallet Nix build
#
# Derivation attributes of this file can be build with "nix-build -A ..."
# Discover attribute names using tab-completion in your shell.
#
# Interesting top-level attributes:
#
#   - bcc-wallet - cli executable
#   - tests - attrset of test-suite executables
#     - bcc-wallet-core.unit
#     - bcc-wallet.integration
#     - etc (layout is PACKAGE.COMPONENT)
#   - checks - attrset of test-suite results
#     - bcc-wallet-core.unit
#     - bcc-wallet.integration
#     - etc
#   - benchmarks - attret of benchmark executables
#     - bcc-wallet-core.db
#     - bcc-wallet.latency
#     - etc
#   - migration-tests - tests db migrations from previous versions
#   - dockerImage - tarball of the docker image
#   - private
#     - shell - import of shell.nix
#     - project.hsPkgs - a Haskell.nix package set of all packages and their dependencies
#       - bcc-wallet-core.components.library
#       - etc (layout is PACKAGE-NAME.components.COMPONENT-TYPE.COMPONENT-NAME)
#
# The attributes of this file are imported by the Hydra jobset and
# mapped into the layout TARGET-SYSTEM.ATTR-PATH.BUILD-SYSTEM.
# See release.nix for more info about that.
#
# Other documentation:
#   https://github.com/The-Blockchain-Company/bcc-wallet/wiki/Building#nix-build
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import pinned Nixpkgs with tbco-nix and Haskell.nix overlays
, pkgs ? import ./nix/default.nix { inherit system crossSystem config sourcesOverride; }
# Use this git revision for stamping executables
, gitrev ? pkgs.commonLib.commitIdFromGitRepoOrZero ./.git
# Use this to reference local sources rather than the niv pinned versions (see nix/default.nix)
, sourcesOverride ? {}
# GitHub PR number (as a string), set when building a Hydra PR jobset.
, pr ? null
# Bors job type (as a string), set when building a Hydra bors jobset.
, borsBuild ? null
}:

let
  # The project sources. Sources are filtered by filename, and then
  # further filtered by package subdirectory.
  src = lib.cleanSourceWith {
    src = pkgs.haskell-nix.cleanSourceHaskell { src = ./.; };
    name = "bcc-wallet-src";
    filter = commonLib.removeSocketFilesFilter;
  };

  # Builds the Haskell project with Haskell.nix.
  # There are variants with profiling or test coverage enabled.
  buildHaskellProject = args: import ./nix/haskell.nix ({
    inherit config pkgs;
    inherit (pkgs) buildPackages lib stdenv haskell-nix;
    inherit src gitrev pr borsBuild;
  } // args);
  project = buildHaskellProject {};
  profiledProject = buildHaskellProject { profiling = true; };
  coveredProject = buildHaskellProject { coverage = true; };

  # Functions used in this file
  inherit (pkgs) lib commonLib;
  inherit (pkgs.haskell-nix.haskellLib)
    selectProjectPackages
    isProjectPackage
    collectComponents
    collectChecks;
  getPackageChecks = lib.mapAttrs (_: package: package.checks);

  self = {
    inherit (project.hsPkgs.bcc-wallet-core.identifier) version;
    inherit src;

    # Bcc wallet
    bcc-wallet = import ./nix/release-build.nix {
      inherit pkgs gitrev;
      exe = project.hsPkgs.bcc-wallet.components.exes.bcc-wallet;
      backend = self.bcc-node;
    };
    # Local test cluster and mock metadata server
    inherit (project.hsPkgs.bcc-wallet.components.exes)
      local-cluster
      mock-token-metadata-server;

    # Adrestia tool belt
    inherit (project.hsPkgs.bech32.components.exes) bech32;
    inherit (project.hsPkgs.bcc-addresses-cli.components.exes) bcc-address;

    # Bcc
    inherit (project.hsPkgs.bcc-cli.components.exes) bcc-cli;
    bcc-node = project.hsPkgs.bcc-node.components.exes.bcc-node // {
      deployments = pkgs.bcc-node-deployments;
    };

    # Provide db-converter, so klarity can ship it without needing to
    # pin an ouroborus-network rev.
    inherit (project.hsPkgs.shardagnostic-consensus-cole.components.exes)
      db-converter;

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isProjectPackage coveredProject.hsPkgs;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (selectProjectPackages coveredProject.hsPkgs));
    # Combined project coverage report
    testCoverageReport = coveredProject.projectCoverageReport;
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isProjectPackage project.hsPkgs;

    # See the imported file for how to use the docker build.
    dockerImage = pkgs.callPackage ./nix/docker.nix {
      exes = with self; [ bcc-wallet local-cluster ];
      base = with self; [
        bech32
        bcc-address
        bcc-cli
        bcc-node
        (pkgs.linkFarm "docker-config-layer" [ { name = "config"; path = pkgs.bcc-node-deployments; } ])
      ];
    };

    # These attributes are part of the project build, but are
    # considered to be implementation details, not public API.
    # It's possible to use them, but they may change at any time
    # without warning.
    private = pkgs.recurseIntoAttrs {
      inherit pkgs project profiledProject coveredProject;

      # Shells
      shell = import ./shell.nix { inherit pkgs; walletPackages = self; };
      shell-prof = import ./shell.nix { inherit pkgs; walletPackages = self; profiling = true; };
      cabalShell = import ./nix/cabal-shell.nix { inherit pkgs; walletPackages = self; };
      stackShell = import ./nix/stack-shell.nix { inherit pkgs; walletPackages = self; };

      # This is the ./nix/regenerate.sh script. Put it here so that it's
      # built and cached on CI.
      inherit (pkgs) stackNixRegenerate;

      # Ensure that the project's eval-time GC roots are built and
      # cached by CI.
      roots = pkgs.recurseIntoAttrs {
        project = project.roots;
        tbco-nix-utils = pkgs.tbco-nix-utils.roots;
      };
    };
  };

in
  self
