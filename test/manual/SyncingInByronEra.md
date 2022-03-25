# Tests for syncing in the Cole era

## OS

Windows, MacOS, Linux

## Restart syncing in Cole era.

1. Start bcc-node and bcc-wallet on `testnet` using latest [config](https://hydra.tbco.io/job/Bcc/tbco-nix/bcc-deployment/latest/download/1/index.html). **Start syncing from scratch.**

```bash
$ bcc-node run \
		--config ./*-config.json \
		--topology ./*-topology.json \
		--database-path ./db \
		--socket-path ./node.socket

$ bcc-wallet serve --port 8090 \
		--node-socket ../relay1/node.socket \
		--testnet testnet-cole-genesis.json  \
		--database ./wallet-db
```

2. Note the `sync_progress` while wallet and node are syncing through Cole era.

```bash
$ bcc-wallet network information | jq .sync_progress

Ok.
{
  "status": "syncing",
  "progress": {
    "quantity": 49.95,
    "unit": "percent"
  }
}
```

3. Stop the bcc-wallet, but **keep bcc-node running**.


4. Start bcc-wallet again and make sure that `sync_progress` restarts from the place it was stopped at and moves forward.

```bash
$ bcc-wallet network information | jq .sync_progress

Ok.
{
  "status": "syncing",
  "progress": {
    "quantity": 49.95,
    "unit": "percent"
  }
}
```

## Listing stake pools


6. List stake pools while the wallet is syncing through Cole era and make sure appropriate error message is presented.

```bash
$ curl -X GET 'http://localhost:8090/v2/stake-pools?stake=1000000000'

{
  "code": "past_horizon",
  "message": "Tried to convert something that is past the horizon (due to uncertainty about the next hard fork). Wait for the node to finish syncing to the hard fork. Depending on the blockchain, this process can take an unknown amount of time."
}
```
