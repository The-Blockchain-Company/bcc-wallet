#!/usr/bin/env bash

set -euo pipefail

if type -p gdate > /dev/null; then
  gnu_date=gdate
else
  gnu_date=date
fi

systemStart=$($gnu_date --iso-8601=s --date="5 seconds")

config_dir=lib/sophie/test/data/bcc-node-sophie

mkdir -p ${state_dir:=bft-node}

yq -y '. + { GenesisFile: "genesis.json", minSeverity: "Info" }' < $config_dir/node.config > $state_dir/node.config

yq ".systemStart=\"$(date --iso-8601=s --date='5 seconds')\"" < $config_dir/genesis.yaml > $state_dir/genesis.json

set -x

exec bcc-node run --port 40000 \
     --config $state_dir/node.config \
     --topology lib/cole/test/data/bcc-node-cole/node.topology \
     --database-path $state_dir/node.db \
     --socket-path $state_dir/node.socket \
     --sophie-vrf-key $config_dir/node-vrf.skey \
     --sophie-kes-key $config_dir/node-kes.skey \
     --sophie-operational-certificate $config_dir/node.opcert
