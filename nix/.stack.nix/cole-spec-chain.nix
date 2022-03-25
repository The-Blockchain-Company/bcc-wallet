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
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cole-spec-chain"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@tbco.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/The-Blockchain-Company/bcc-legder-specs";
      url = "";
      synopsis = "Executable specification of the Bcc blockchain";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cole-spec-ledger" or (errorHandler.buildDepError "cole-spec-ledger"))
          (hsPkgs."shepards" or (errorHandler.buildDepError "shepards"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
          ];
        buildable = true;
        };
      tests = {
        "chain-rules-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-ordlist" or (errorHandler.buildDepError "data-ordlist"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."cole-spec-chain" or (errorHandler.buildDepError "cole-spec-chain"))
            (hsPkgs."cole-spec-ledger" or (errorHandler.buildDepError "cole-spec-ledger"))
            (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
            (hsPkgs."small-steps-test" or (errorHandler.buildDepError "small-steps-test"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/The-Blockchain-Company/bcc-ledger-specs";
      rev = "f827a4321e42f528e25f6079f7af3eb18f10d391";
      sha256 = "0dmgxg7cpgz4lnscqrrk4gakw9w90dx8ljv5wr923rfp9nyzc5qf";
      }) // {
      url = "https://github.com/The-Blockchain-Company/bcc-ledger-specs";
      rev = "f827a4321e42f528e25f6079f7af3eb18f10d391";
      sha256 = "0dmgxg7cpgz4lnscqrrk4gakw9w90dx8ljv5wr923rfp9nyzc5qf";
      };
    postUnpack = "sourceRoot+=/cole/chain/executable-spec; echo source root reset to \$sourceRoot";
    }