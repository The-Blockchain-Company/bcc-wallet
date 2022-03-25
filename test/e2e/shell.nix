{ pkgs ? import ../../nix/default.nix {}
, bccWallet ? import ../../default.nix { inherit pkgs; }
# Whether to build bcc-wallet from this source directory and
# include in the shell.
, bins ? true
}:

let
  # To update gemset.nix, run:
  #   nix-shell --arg bins false --run bundix
  gems = pkgs.bundlerEnv {
    name = "gems-bcc-wallet-e2e";
    gemdir = ./.;
    ruby = pkgs.ruby_2_7;
  };
in pkgs.mkShell {
  name = "bcc-wallet-e2e";
  buildInputs = [
    gems
    gems.wrappedRuby
    pkgs.bundix
    pkgs.screen
  ] ++ pkgs.lib.optionals bins [
    bccWallet.bcc-wallet
    bccWallet.bcc-node
  ];
  BCC_NODE_CONFIGS = pkgs.bcc-node-deployments;
}
