pkgs: _: {
  # Provide real deployment configurations for use in dev/tests/benchmarks.
  # https://hydra.tbco.io/job/Bcc/tbco-nix/bcc-deployment/latest/download/1/index.html
  bcc-node-deployments = let
    environments = {
      inherit (pkgs.commonLib.bccLib.environments)
        mainnet
        staging
        testnet
        sophie_qa
        aurum-purple
        ;
    };
    updateConfig = env: env.nodeConfig // {
      minSeverity = "Notice";
    } // (if (env.consensusProtocol == "Bcc") then {
      ColeGenesisFile = "genesis-cole.json";
      SophieGenesisFile = "genesis-sophie.json";
      AurumGenesisFile = "genesis-aurum.json";
    } else {
      GenesisFile = "genesis.json";
    });
    mkTopology = env: pkgs.commonLib.bccLib.mkEdgeTopology {
      edgeNodes = [ env.relaysNew ];
      valency = 2;
    };
    mapAttrsToString = f: attrs:
      pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList f attrs);
  in
    pkgs.runCommand "bcc-node-deployments" {
      nativeBuildInputs = [ pkgs.buildPackages.jq ];
    } (mapAttrsToString (name: env: ''
      cfg=$out/${name}
      mkdir -p $cfg
      jq . < ${mkTopology env} > $cfg/topology.json
      jq . < ${__toFile "${name}-config.json" (__toJSON (updateConfig env))} > $cfg/configuration.json
    '' + (if env.consensusProtocol == "Bcc" then ''
      jq . < ${env.nodeConfig.ColeGenesisFile} > $cfg/genesis-cole.json
      cp ${env.nodeConfig.SophieGenesisFile} $cfg/genesis-sophie.json
      cp ${env.nodeConfig.AurumGenesisFile} $cfg/genesis-aurum.json
    '' else ''
      jq . < ${env.genesisFile} > $cfg/genesis.json
    '')) environments);
}
