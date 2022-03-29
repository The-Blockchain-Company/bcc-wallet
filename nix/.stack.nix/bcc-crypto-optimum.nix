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
    flags = { development = false; external-libsodium-vrf = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "bcc-crypto-optimum"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 TBCO";
      maintainer = "operations@blockchain-company.io";
      author = "TBCO";
      homepage = "";
      url = "";
      synopsis = "Crypto primitives from libsodium";
      description = "VRF (and KES, tba) primitives from libsodium.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/The-Blockchain-Company/bcc-base";
      rev = "e8a48cf0500b03c744c7fc6f2fedb86e8bdbe055";
      sha256 = "0s3w796y4bgjidg5iwapdq88cq9ipy346gagbip6xlqxdvpp99xj";
      }) // {
      url = "https://github.com/The-Blockchain-Company/bcc-base";
      rev = "e8a48cf0500b03c744c7fc6f2fedb86e8bdbe055";
      sha256 = "0s3w796y4bgjidg5iwapdq88cq9ipy346gagbip6xlqxdvpp99xj";
      };
    postUnpack = "sourceRoot+=/bcc-crypto-optimum; echo source root reset to \$sourceRoot";
    }