RSpec.describe BccWallet::Cole do

  describe BccWallet::Cole::Wallets do

    after(:each) do
      teardown
    end

    it "I can list cole wallets" do
      l = COLE.wallets.list
      expect(l).to be_correct_and_respond 200
    end

    it "I could get a wallet" do
      g = COLE.wallets.get "db66f3d0d796c6aa0ad456a36d5a3ee88d62bd5d"
      expect(g).to be_correct_and_respond 404
    end

    it "I could delete a wallet" do
      g = COLE.wallets.delete "db66f3d0d796c6aa0ad456a36d5a3ee88d62bd5d"
      expect(g).to be_correct_and_respond 404
    end

    it "I can create, get and delete cole icarus wallet from mnemonics" do
      wallet = COLE.wallets.create({ style: "icarus",
                         name: "Wallet from mnemonic_sentence",
                         passphrase: "Secure Passphrase",
                         mnemonic_sentence: mnemonic_sentence(15),
                         })
      expect(wallet).to be_correct_and_respond 201

      wid = wallet['id']
      g = COLE.wallets.get(wid)
      expect(g).to be_correct_and_respond 200
      expect(COLE.wallets.delete(wid)).to be_correct_and_respond 204
    end

    it "I can create, get and delete cole random wallet from mnemonics" do
      wallet = COLE.wallets.create({ style: "random",
                         name: "Wallet from mnemonic_sentence",
                         passphrase: "Secure Passphrase",
                         mnemonic_sentence: mnemonic_sentence(12),
                         })
      expect(wallet).to be_correct_and_respond 201


      wid = wallet['id']
      g = COLE.wallets.get(wid)
      expect(g).to be_correct_and_respond 200


      expect(COLE.wallets.delete(wid)).to be_correct_and_respond 204
    end

    it "Can update_metadata" do
      w = COLE.wallets
      id = create_cole_wallet
      u = w.update_metadata(id, { name: "New wallet name" })
      expect(u).to be_correct_and_respond 200

    end

    it "Can update_passphrase" do
      w = COLE.wallets
      id = create_cole_wallet
      upd = w.update_passphrase(id, { old_passphrase: "Secure Passphrase",
                                    new_passphrase: "Securer Passphrase" })
      expect(upd).to be_correct_and_respond 204
    end

    it "Can see utxo" do
      id = create_cole_wallet
      utxo = COLE.wallets.utxo(id)
      expect(utxo).to be_correct_and_respond 200
    end

    it "Can see utxo snapshot" do
      id = create_cole_wallet
      utxo = COLE.wallets.utxo_snapshot(id)
      expect(utxo).to be_correct_and_respond 200
    end
  end

  describe BccWallet::Cole::Addresses do

    after(:each) do
      teardown
    end

    it "Can list addresses - random" do
      id = create_cole_wallet
      addresses = COLE.addresses.list id
      expect(addresses).to be_correct_and_respond 200

      expect(addresses.size).to eq 0

      COLE.addresses.create(id, { passphrase: PASS })
      addresses = COLE.addresses.list id
      expect(addresses).to be_correct_and_respond 200

      expect(addresses.size).to eq 1
      expect(addresses.first['derivation_path'][0]).to eq '0H'
      expect(addresses.first['derivation_path'][1]).to end_with 'H'
    end

    it "Can list addresses - icarus" do
      id = create_cole_wallet "icarus"
      addresses_unused = COLE.addresses.list id, { state: "unused" }
      expect(addresses_unused).to be_correct_and_respond 200

      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['44H', '1815H', '0H', '0', i.to_s]
      end
    end

    it "Can list addresses - ledger" do
      id = create_cole_wallet "ledger"
      addresses_unused = COLE.addresses.list id, { state: "unused" }
      expect(addresses_unused).to be_correct_and_respond 200

      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['44H', '1815H', '0H', '0', i.to_s]
      end
    end

    it "Can list addresses - trezor" do
      id = create_cole_wallet "trezor"
      addresses_unused = COLE.addresses.list id, { state: "unused" }
      expect(addresses_unused).to be_correct_and_respond 200

      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['44H', '1815H', '0H', '0', i.to_s]
      end
    end

    it "Can create address - random" do
      id = create_cole_wallet
      addr = COLE.addresses.create(id, { passphrase: PASS,
                                         address_index: 2147483648 })
      expect(addr).to be_correct_and_respond 201
      expect(addr['derivation_path']).to eq ['0H', '0H']

      addr_r = COLE.addresses.create(id, { passphrase: PASS })
      expect(addr_r).to be_correct_and_respond 201
      expect(addr_r['derivation_path'][0]).to eq '0H'
      expect(addr_r['derivation_path'][1]).to end_with 'H'
    end

    it "I can import address - random" do
      id = create_fixture_cole_wallet
      addr = '37btjrVyb4KEciULDrqJDBh6SjgPqi9JJ5qQqWGgvtsB7GcsuqorKceMTBRudFK8zDu3btoC5FtN7K1PEHmko4neQPfV9TDVfivc9JTZVNPKtRd4w2'
      addr_import = COLE.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 204

      addresses = COLE.addresses.list id
      expect(addresses).to be_correct_and_respond 200
      expect(addresses.size).to eq 1
      expect(addresses.first['derivation_path']).to eq ['0H', '2147483647H']
    end

    it "I cannot import address - icarus" do
      id = create_cole_wallet "icarus"
      addr = COLE.addresses.list(id)[0]['id']
      addr_import = COLE.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 403
      expect(addr_import.to_s).to include "invalid_wallet_type"
    end

    it "I cannot import address - ledger" do
      id = create_cole_wallet "ledger"
      addr = COLE.addresses.list(id)[0]['id']
      addr_import = COLE.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 403
      expect(addr_import.to_s).to include "invalid_wallet_type"
    end

    it "I cannot import address - trezor" do
      id = create_cole_wallet "trezor"
      addr = COLE.addresses.list(id)[0]['id']
      addr_import = COLE.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 403
      expect(addr_import.to_s).to include "invalid_wallet_type"
    end
  end

  describe BccWallet::Cole::CoinSelections do

    after(:each) do
      teardown
    end

    it "I could trigger random coin selection - if had money" do
      wid = create_cole_wallet "icarus"
      addresses = COLE.addresses.list(wid)
      addr_amount = [
         { addresses[0]['id'] => 123 },
         { addresses[1]['id'] => 456 }
        ]

      rnd = COLE.coin_selections.random wid, addr_amount

      expect(rnd).to be_correct_and_respond 403
      expect(rnd.to_s).to include "not_enough_money"
    end

  end

  describe BccWallet::Cole::Transactions do
    after(:each) do
      teardown
    end

    # Run for random and icarus
    ["random", "icarus"].each do |style|

      it "I could get a tx if I had proper id - #{style}" do
        wid = create_cole_wallet style
        txs = COLE.transactions
        g = txs.get(wid, TXID)
        expect(g).to be_correct_and_respond 404
        expect(g.to_s).to include "no_such_transaction"
      end

      it "Can list transactions - #{style}" do
        id = create_cole_wallet style
        txs = COLE.transactions

        expect(txs.list(id)).to be_correct_and_respond 200
        expect(txs.list(id,
                        { start: "2012-09-25T10:15:00Z",
                         end: "2016-11-21T10:15:00Z",
                         order: "ascending" }).code).
                        to eq 200
        expect(txs.list(id, { order: "bad_order" })).to be_correct_and_respond 400
      end

      it "I could send tx if I had money - #{style}" do
        id = create_cole_wallet style
        target_id = create_cole_wallet "icarus"
        target_addr = COLE.addresses.list(target_id)[0]['id']

        tx_sent = COLE.transactions.create(id, PASS, [{ target_addr => 1000000 }])
        expect(tx_sent).to be_correct_and_respond 403
        expect(tx_sent.to_s).to include "not_enough_money"
      end

      it "I could estimate fees if I had money - #{style}" do
        id = create_cole_wallet style
        target_id = create_cole_wallet "icarus"
        target_addr = COLE.addresses.list(target_id)[0]['id']

        fees = COLE.transactions.payment_fees(id, [{ target_addr => 1000000 }])
        expect(fees).to be_correct_and_respond 403
        expect(fees.to_s).to include "not_enough_money"
      end

      it "I could forget transaction - #{style}" do
        id = create_cole_wallet style
        txs = COLE.transactions
        res = txs.forget(id, TXID)
        expect(res).to be_correct_and_respond 404
      end
    end
  end

  describe BccWallet::Cole::Migrations do
    after(:each) do
      teardown
    end

    it "I could create migration plan - icarus" do
      id = create_cole_wallet "icarus"
      target_id = create_sophie_wallet
      addrs = SOPHIE.addresses.list(target_id).map { |a| a['id'] }

      plan = COLE.migrations.plan(id, addrs)
      expect(plan).to be_correct_and_respond 403
      expect(plan.to_s).to include "nothing_to_migrate"
    end

    it "I could create migration plan - random" do
      id = create_cole_wallet "random"
      target_id = create_sophie_wallet
      addrs = SOPHIE.addresses.list(target_id).map { |a| a['id'] }

      plan = COLE.migrations.plan(id, addrs)
      expect(plan).to be_correct_and_respond 403
      expect(plan.to_s).to include "nothing_to_migrate"
    end

    it "I could migrate all my funds" do
      id = create_cole_wallet "random"
      target_wal_id = create_cole_wallet "icarus"
      addresses = COLE.addresses.list(target_wal_id).map { |a| a['id'] }
      migr = COLE.migrations.migrate(id, PASS, addresses)
      expect(migr).to be_correct_and_respond 403
      expect(migr.to_s).to include "nothing_to_migrate"
    end
  end
end
