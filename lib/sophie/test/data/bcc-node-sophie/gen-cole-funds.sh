#!/usr/bin/env bash

set -euo pipefail

# Script for generating faucet addresses for some of the integration test
# setup that relies on the CLI.
#
# Generates cole key file, sophie key file, address.
# The addresses can be inserted to the cole genesis.
# The corresponding TxIn will then be blake2d256 of the address.
cd `dirname $0`
dir=faucet-addrs
rm -f $dir/*
for i in $(seq 200);
do
  # Generate payment key and addresses for payment and genesis utxo
  bcc-cli cole key keygen --secret $dir/faucet$i.cole.key
  bcc-cli \
    cole key signing-key-address \
    --cole-formats \
    --mainnet \
    --secret $dir/faucet$i.cole.key \
    | head -n 1 > $dir/faucet$i.addr

  bcc-cli key convert-cole-key \
    --cole-payment-key-type \
    --cole-signing-key-file $dir/faucet$i.cole.key \
    --out-file $dir/faucet$i.sophie.key
done
