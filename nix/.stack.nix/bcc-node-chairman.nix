{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "bcc-node-chairman"; version = "1.27.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@tbco.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "The bcc full node";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      exes = {
        "bcc-node-chairman" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-api" or (errorHandler.buildDepError "bcc-api"))
            (hsPkgs."bcc-config" or (errorHandler.buildDepError "bcc-config"))
            (hsPkgs."bcc-ledger-cole" or (errorHandler.buildDepError "bcc-ledger-cole"))
            (hsPkgs."bcc-node" or (errorHandler.buildDepError "bcc-node"))
            (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."shardagnostic-consensus" or (errorHandler.buildDepError "shardagnostic-consensus"))
            (hsPkgs."shardagnostic-network" or (errorHandler.buildDepError "shardagnostic-network"))
            (hsPkgs."shardagnostic-network-framework" or (errorHandler.buildDepError "shardagnostic-network-framework"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        };
      tests = {
        "chairman-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bcc-testnet" or (errorHandler.buildDepError "bcc-testnet"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-extras" or (errorHandler.buildDepError "hedgehog-extras"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.bcc-node.components.exes.bcc-node or (pkgs.buildPackages.bcc-node or (errorHandler.buildToolDepError "bcc-node:bcc-node")))
            (hsPkgs.buildPackages.bcc-cli.components.exes.bcc-cli or (pkgs.buildPackages.bcc-cli or (errorHandler.buildToolDepError "bcc-cli:bcc-cli")))
            (hsPkgs.buildPackages.bcc-node-chairman.components.exes.bcc-node-chairman or (pkgs.buildPackages.bcc-node-chairman or (errorHandler.buildToolDepError "bcc-node-chairman:bcc-node-chairman")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/The-Blockchain-Company/bcc-node";
      rev = "bc225ae3085ba6f4f4007c50c4877bc4cebcd7de";
      sha256 = "18i3axv3irls5gbcsppkw1aci72zyqw80ffhz25sfpbflbj1m4f5";
      });
    postUnpack = "sourceRoot+=/bcc-node-chairman; echo source root reset to \$sourceRoot";
    }