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
    flags = { systemd = true; };
    package = {
      specVersion = "3.0";
      identifier = { name = "bcc-config"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@tbco.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/The-Blockchain-Company/bcc-node";
      rev = "0fb43f4e3da8b225f4f86557aed90a183981a64f";
      sha256 = "0mkir1pg78hp7adxgb8cz6jj4izs07np23fxxnwhkvf0ql92nan7";
      }) // {
      url = "https://github.com/The-Blockchain-Company/bcc-node";
      rev = "0fb43f4e3da8b225f4f86557aed90a183981a64f";
      sha256 = "0mkir1pg78hp7adxgb8cz6jj4izs07np23fxxnwhkvf0ql92nan7";
      };
    postUnpack = "sourceRoot+=/bcc-config; echo source root reset to \$sourceRoot";
    }