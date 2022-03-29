{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { release = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "bcc-addresses-cli"; version = "3.6.0"; };
      license = "Apache-2.0";
      copyright = "2021 TBCO";
      maintainer = "operations@blockchain-company.io";
      author = "TBCO";
      homepage = "https://github.com/The-Blockchain-Company/bcc-addresses#readme";
      url = "";
      synopsis = "Utils for constructing a command-line on top of bcc-addresses.";
      description = "Please see the README on GitHub at <https://github.com/The-Blockchain-Company/bcc-addresses>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-addresses" or (errorHandler.buildDepError "bcc-addresses"))
          (hsPkgs."bcc-crypto" or (errorHandler.buildDepError "bcc-crypto"))
          (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      exes = {
        "bcc-address" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bcc-addresses" or (errorHandler.buildDepError "bcc-addresses"))
            (hsPkgs."bcc-addresses-cli" or (errorHandler.buildDepError "bcc-addresses-cli"))
            ];
          buildable = true;
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."bcc-addresses" or (errorHandler.buildDepError "bcc-addresses"))
            (hsPkgs."bcc-addresses-cli" or (errorHandler.buildDepError "bcc-addresses-cli"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."string-interpolate" or (errorHandler.buildDepError "string-interpolate"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhcjs && true || system.isGhcjs)) (hsPkgs."hjsonschema" or (errorHandler.buildDepError "hjsonschema"));
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            (hsPkgs.buildPackages.bcc-address.components.exes.bcc-address or (pkgs.buildPackages.bcc-address or (errorHandler.buildToolDepError "bcc-address:bcc-address")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/The-Blockchain-Company/bcc-addresses";
      rev = "4003fc09780da61bc09d85337bdd4c7664aa49ba";
      sha256 = "0ffavab3h56p1b1narn1w8aq4wr2affdl5xyz8cxv5ighi1sw98g";
      }) // {
      url = "https://github.com/The-Blockchain-Company/bcc-addresses";
      rev = "4003fc09780da61bc09d85337bdd4c7664aa49ba";
      sha256 = "0ffavab3h56p1b1narn1w8aq4wr2affdl5xyz8cxv5ighi1sw98g";
      };
    postUnpack = "sourceRoot+=/command-line; echo source root reset to \$sourceRoot";
    }) // { cabal-generator = "hpack"; }