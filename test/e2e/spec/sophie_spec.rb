RSpec.describe BccWallet::Sophie do

  describe BccWallet::Sophie::Wallets do

    after(:each) do
      teardown
    end

    it "I can list wallets" do
      l = SOPHIE.wallets.list
      expect(l).to be_correct_and_respond 200
      expect(l.size).to eq 0

      create_sophie_wallet
      l = SOPHIE.wallets.list
      expect(l).to be_correct_and_respond 200
      expect(l.size).to eq 1
    end

    it "When wallet does not exist it gives 404" do
      wid = create_sophie_wallet
      SOPHIE.wallets.delete wid
      g = SOPHIE.wallets.get wid
      expect(g).to be_correct_and_respond 404

      d = SOPHIE.wallets.delete wid
      expect(d).to be_correct_and_respond 404
    end

    describe "Create wallets" do
      it "I can create, get and delete wallet from mnemonics" do
        w = SOPHIE.wallets
        wallet = w.create({ name: "Wallet from mnemonic_sentence",
                           passphrase: "Secure Passphrase",
                           mnemonic_sentence: mnemonic_sentence(15),
                           })
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = w.get(wid)
        expect(g).to be_correct_and_respond 200

        expect(w.delete(wid)).to be_correct_and_respond 204
      end

      it "I can create, get and delete wallet from mnemonics / second factor" do
        w = SOPHIE.wallets
        wallet = w.create({ name: "Wallet from mnemonic_sentence",
                           passphrase: "Secure Passphrase",
                           mnemonic_sentence: mnemonic_sentence(15),
                           mnemonic_second_factor: mnemonic_sentence(12)
                           })
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = w.get(wid)
        expect(g).to be_correct_and_respond 200
        expect(w.delete(wid)).to be_correct_and_respond 204
      end

      it "I can set address pool gap" do
        pool_gap = 55
        w = SOPHIE.wallets
        wallet = w.create({ name: "Wallet from mnemonic_sentence",
                           passphrase: "Secure Passphrase",
                           mnemonic_sentence: mnemonic_sentence(15),
                           address_pool_gap: pool_gap
                           })
        expect(wallet).to be_correct_and_respond 201
        addr = SOPHIE.addresses.list(wallet['id'])
        expect(addr).to be_correct_and_respond 200
        expect(addr.size).to eq pool_gap
      end

      it "I can create, get and delete wallet from pub key" do
        w = SOPHIE.wallets
        wallet = w.create({ name: "Wallet from pub key",
                           account_public_key: "b47546e661b6c1791452d003d375756dde6cac2250093ce4630f16b9b9c0ac87411337bda4d5bc0216462480b809824ffb48f17e08d95ab9f1b91d391e48e66b",
                           address_pool_gap: 20,
                           })
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = w.get(wid)
        expect(g).to be_correct_and_respond 200
        expect(w.delete(wid)).to be_correct_and_respond 204
      end
    end

    describe "Update wallet" do
      it "Can update_metadata" do
        new_name = "New wallet name"
        w = SOPHIE.wallets
        id = create_sophie_wallet
        u = w.update_metadata(id, { name: new_name })
        expect(u).to be_correct_and_respond 200
        expect(w.get(id)['name']).to eq new_name
      end

      it "Can update_passphrase" do
        w = SOPHIE.wallets
        id = create_sophie_wallet
        upd = w.update_passphrase(id, { old_passphrase: "Secure Passphrase",
                                      new_passphrase: "Securer Passphrase" })
        expect(upd).to be_correct_and_respond 204
      end
    end

    it "Can see utxo" do
      id = create_sophie_wallet
      utxo = SOPHIE.wallets.utxo(id)
      expect(utxo).to be_correct_and_respond 200
    end

    it "Can see utxo snapshot" do
      id = create_sophie_wallet
      utxo = SOPHIE.wallets.utxo_snapshot(id)
      expect(utxo).to be_correct_and_respond 200
    end
  end

  describe BccWallet::Sophie::Addresses do

    after(:each) do
      teardown
    end

    it "Can list addresses" do
      id = create_sophie_wallet
      sophie_addr = BccWallet.new.sophie.addresses
      addresses = sophie_addr.list id
      expect(addresses).to be_correct_and_respond 200
      expect(addresses.size).to eq 20
      addresses.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['1852H', '1815H', '0H', '0', i.to_s]
      end

      addresses_unused = sophie_addr.list id, { state: "used" }
      expect(addresses_unused).to be_correct_and_respond 200
      expect(addresses_unused.size).to eq 0

      addresses_unused = sophie_addr.list id, { state: "unused" }
      expect(addresses_unused).to be_correct_and_respond 200
      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['1852H', '1815H', '0H', '0', i.to_s]
      end
    end
  end

  describe BccWallet::Sophie::CoinSelections do

    after(:each) do
      teardown
    end

    it "I could trigger random coin selection - if had money" do
      wid = create_sophie_wallet
      addresses = SOPHIE.addresses.list(wid)
      addr_amount = [
         { addresses[0]['id'] => 123 },
         { addresses[1]['id'] => 456 }
        ]

      rnd = SOPHIE.coin_selections.random wid, addr_amount
      expect(rnd).to be_correct_and_respond 403
      expect(rnd.to_s).to include "not_enough_money"
    end
  end

  describe BccWallet::Sophie::Transactions do

    after(:each) do
      teardown
    end

    it "I could get a tx if I had proper id" do
      wid = create_sophie_wallet
      txs = SOPHIE.transactions
      g = txs.get(wid, TXID)
      expect(g).to be_correct_and_respond 404
      expect(g.to_s).to include "no_such_transaction"
    end

    it "Can list transactions" do
      id = create_sophie_wallet
      txs = SOPHIE.transactions
      l = txs.list(id)
      l_ext = txs.list(id, { start: "2012-09-25T10:15:00Z",
                           end: "2016-11-21T10:15:00Z",
                           order: "ascending" })
      l_bad = txs.list(id, { order: "bad_order" })
      expect(l).to be_correct_and_respond 200
      expect(l_ext).to be_correct_and_respond 200
      expect(l_bad).to be_correct_and_respond 400
    end

    it "I could create transaction - if I had money" do
      id = create_sophie_wallet
      target_id = create_sophie_wallet
      address = SOPHIE.addresses.list(target_id)[0]['id']
      txs = SOPHIE.transactions
      amt = [{ address => 1000000 }]

      tx_sent = txs.create(id, PASS, amt)
      expect(tx_sent).to be_correct_and_respond 403
      expect(tx_sent.to_s).to include "not_enough_money"
    end

    it "I could create transaction using rewards - if I had money" do
      id = create_sophie_wallet
      target_id = create_sophie_wallet
      address = SOPHIE.addresses.list(target_id)[0]['id']
      txs = SOPHIE.transactions
      amt = [{ address => 1000000 }]

      tx_sent = txs.create(id, PASS, amt, 'self')
      expect(tx_sent).to be_correct_and_respond 403
      expect(tx_sent.to_s).to include "not_enough_money"
    end

    it "I could estimate transaction fee - if I had money" do
      id = create_sophie_wallet
      target_id = create_sophie_wallet
      address = SOPHIE.addresses.list(target_id)[0]['id']
      amt = [{ address => 1000000 }]

      txs = SOPHIE.transactions

      fees = txs.payment_fees(id, amt)
      expect(fees).to be_correct_and_respond 403
      expect(fees.to_s).to include "not_enough_money"

      fees = txs.payment_fees(id, amt, 'self')
      expect(fees).to be_correct_and_respond 403
      expect(fees.to_s).to include "not_enough_money"

      metadata = { "0" => { "string" => "bcc" },
                   "1" => { "int" => 14 },
                   "2" => { "bytes" => "2512a00e9653fe49a44a5886202e24d77eeb998f" },
                   "3" => { "list" => [ { "int" => 14 }, { "int" => 42 }, { "string" => "1337" } ] },
                   "4" => { "map" => [ { "k" => { "string" => "key" }, "v" => { "string" => "value" } },
                                   { "k" => { "int" => 14 }, "v" => { "int" => 42 } } ] } }

      fees = txs.payment_fees(id, amt, 'self', metadata)
      expect(fees).to be_correct_and_respond 403
      expect(fees.to_s).to include "not_enough_money"
    end

    it "I could forget transaction" do
      id = create_sophie_wallet
      txs = SOPHIE.transactions
      res = txs.forget(id, TXID)
      expect(res).to be_correct_and_respond 404
    end
  end

  describe BccWallet::Sophie::StakePools do

    after(:each) do
      settings = BccWallet.new.misc.settings
      s = settings.update({ :pool_metadata_source => "none" })
      teardown
    end

    it "I can list stake keys" do
      id = create_sophie_wallet
      stake_keys = SOPHIE.stake_pools.list_stake_keys(id)
      expect(stake_keys).to be_correct_and_respond 200
      expect(stake_keys['foreign'].size).to eq 0
      expect(stake_keys['ours'].size).to eq 1
      expect(stake_keys['ours'].first['stake']).to eq({ "quantity" => 0, "unit" => "entropic" })
      expect(stake_keys['none']['stake']).to eq({ "quantity" => 0, "unit" => "entropic" })
      expect(stake_keys['ours'].first['delegation']).to eq({ "next" => [],
                                                             "active" =>
                                                             { "status" => "not_delegating" } })
    end

    it "ADP-634 - Pool metadata is updated when settings are updated" do
      settings = BccWallet.new.misc.settings
      pools = SOPHIE.stake_pools

      s = settings.update({ :pool_metadata_source => "direct" })
      expect(s).to be_correct_and_respond 204

      eventually "Pools have metadata when 'pool_metadata_source' => 'direct'" do
        sps = pools.list({ stake: 1000 })
        sps.select { |p| p['metadata'] }.size > 0
      end

      s = settings.update({ :pool_metadata_source => "none" })
      expect(s).to be_correct_and_respond 204

      eventually "Pools have no metadata when 'pool_metadata_source' => 'none'" do
        sps = pools.list({ stake: 1000 })
        sps.select { |p| p['metadata'] }.size == 0
      end

      s = settings.update({ :pool_metadata_source => ENV['TESTS_E2E_SMASH'] })
      expect(s).to be_correct_and_respond 204

      eventually "Pools have metadata when 'pool_metadata_source' => '#{ENV['TESTS_E2E_SMASH']}'" do
        sps = pools.list({ stake: 1000 })
        sps.select { |p| p['metadata'] }.size > 0
      end

      s = settings.update({ :pool_metadata_source => "none" })
      expect(s).to be_correct_and_respond 204

      eventually "Pools have no metadata when 'pool_metadata_source' => 'none'" do
        sps = pools.list({ stake: 1000 })
        sps.select { |p| p['metadata'] }.size == 0
      end
    end

    describe "Stake Pools GC Maintenance" do
      matrix = [{ "direct" => "not_applicable" },
                { "none" => "not_applicable" },
                { "https://smash.bcc-testnet.tbcodev.io" => "has_run" }]
      matrix.each do |tc|
        it "GC metadata maintenance action on metadata source #{tc}" do
          settings = BccWallet.new.misc.settings
          pools = SOPHIE.stake_pools

          s = settings.update({ :pool_metadata_source => tc.keys.first })
          expect(s).to be_correct_and_respond 204

          t = pools.trigger_maintenance_actions({ maintenance_action: "gc_stake_pools" })
          expect(t).to be_correct_and_respond 204

          eventually "Maintenance action has status = #{tc.values.first}" do
            r = pools.view_maintenance_actions
            (r.code == 200) && (r.to_s.include? tc.values.first)
          end
        end
      end
    end
    it "I could quit stake pool - if I was delegating" do
      id = create_sophie_wallet

      pools = SOPHIE.stake_pools
      quit = pools.quit(id, PASS)
      expect(quit).to be_correct_and_respond 403
      expect(quit.to_s).to include "not_delegating_to"
    end

  end

  describe BccWallet::Sophie::Migrations do
    after(:each) do
      teardown
    end

    it "I could create migration plan" do
      id = create_sophie_wallet
      target_id = create_sophie_wallet
      addrs = SOPHIE.addresses.list(target_id).map { |a| a['id'] }

      plan = SOPHIE.migrations.plan(id, addrs)
      expect(plan).to be_correct_and_respond 403
      expect(plan.to_s).to include "nothing_to_migrate"
    end

    it "I could migrate all my funds" do
      id = create_sophie_wallet
      target_id = create_sophie_wallet
      addrs = SOPHIE.addresses.list(target_id).map { |a| a['id'] }
      migr = SOPHIE.migrations.migrate(id, PASS, addrs)
      expect(migr).to be_correct_and_respond 403
      expect(migr.to_s).to include "nothing_to_migrate"
    end
  end

  describe BccWallet::Sophie::Keys do
    after(:each) do
      teardown
    end

    it "Get signed metadata" do
      wid = create_sophie_wallet
      ["utxo_internal", "utxo_external", "mutable_account"].each do |role|
        id = [*0..100000].sample
        res = SOPHIE.keys.sign_metadata(wid,
                                        role,
                                        id,
                                        "Secure Passphrase",
                                        { "0" => { "string" => "bcc" } })
        puts "#{wid}/#{role}/#{id}"
        expect(res).to respond_with 200
      end
    end

    it "Get public key" do
      wid = create_sophie_wallet
      ["utxo_internal", "utxo_external", "mutable_account"].each do |role|
        id = [*0..100000].sample
        res = SOPHIE.keys.get_public_key(wid, role, id)
        puts "#{wid}/#{role}/#{id}"
        expect(res).to be_correct_and_respond 200
      end
    end

    it "Create account public key - extended" do
      m24 = mnemonic_sentence(24)
      wid = create_sophie_wallet("Wallet", m24)
      ["0H", "1H", "2147483647H", "44H"].each do |index|
        payload = { passphrase: PASS, format: 'extended' }
        res = SOPHIE.keys.create_acc_public_key(wid, index, payload)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include bcc_address_get_acc_xpub(m24,
                                                                 "1852H/1815H/#{index}",
                                                                 hex = false,
                                                                 "Sophie")
      end
    end

    it "Create account public key - non_extended" do
      m24 = mnemonic_sentence(24)
      wid = create_sophie_wallet("Wallet", m24)
      ["0H", "1H", "2147483647H", "44H"].each do |index|
        payload = { passphrase: PASS, format: 'non_extended' }
        res = SOPHIE.keys.create_acc_public_key(wid, index, payload)
        expect(res.to_s).to include bcc_address_get_acc_xpub(m24,
                                                                 "1852H/1815H/#{index}",
                                                                 hex = false,
                                                                 "Sophie",
                                                                 "--without-chain-code")
      end
    end

    it "Create account public key - extended with purpose" do
      m24 = mnemonic_sentence(24)
      wid = create_sophie_wallet("Wallet", m24)
      ["0H", "1H", "2147483647H", "1854H"].each do |index_purpose|
        payload = { passphrase: PASS, format: 'extended', purpose: index_purpose }
        res = SOPHIE.keys.create_acc_public_key(wid, index_purpose, payload)
        expect(res).to be_correct_and_respond 202
        type_for_bcc_address = index_purpose == "1854H" ? "Shared" : "Sophie"
        expect(res.to_s).to include bcc_address_get_acc_xpub(m24,
                                                                 "#{index_purpose}/1815H/#{index_purpose}",
                                                                 hex = false,
                                                                 type_for_bcc_address)
      end
    end

    it "Create account public key - non_extended with purpose" do
      m24 = mnemonic_sentence(24)
      wid = create_sophie_wallet("Wallet", m24)
      ["0H", "1H", "2147483647H", "1854H"].each do |index_purpose|
        payload = { passphrase: PASS, format: 'non_extended', purpose: index_purpose }
        res = SOPHIE.keys.create_acc_public_key(wid, index_purpose, payload)
        expect(res).to be_correct_and_respond 202
        type_for_bcc_address = index_purpose == "1854H" ? "Shared" : "Sophie"
        expect(res.to_s).to include bcc_address_get_acc_xpub(m24,
                                                                 "#{index_purpose}/1815H/#{index_purpose}",
                                                                 hex = false,
                                                                 type_for_bcc_address,
                                                                 "--without-chain-code")
      end
    end

    it "Get account public key - wallet from mnemonics" do
      wid = create_sophie_wallet
      res = SOPHIE.keys.get_acc_public_key(wid, { format: "extended" })
      expect(res).to be_correct_and_respond 200
      expect(res.to_s).to include "acct_xvk"
    end

    it "Get account public key - wallet from acc pub key" do
      w = SOPHIE.wallets
      wallet = w.create({ name: "Wallet from pub key",
                         account_public_key: "b47546e661b6c1791452d003d375756dde6cac2250093ce4630f16b9b9c0ac87411337bda4d5bc0216462480b809824ffb48f17e08d95ab9f1b91d391e48e66b",
                         address_pool_gap: 20,
                         })
      expect(wallet).to be_correct_and_respond 201

      res = SOPHIE.keys.get_acc_public_key(wallet['id'], { format: "non_extended" })
      expect(res).to be_correct_and_respond 200
      expect(res.to_s).to include "acct_vk"
    end
  end

end
