module Bcc.Wallet.Primitive.Delegation.UTxO
    ( stakeKeyCoinDistr
    ) where

import Prelude

import Bcc.Wallet.Primitive.Types.Address
    ( Address (..) )
import Bcc.Wallet.Primitive.Types.Coin
    ( Coin )
import Bcc.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Bcc.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
import Bcc.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Data.Map
    ( Map )

import qualified Bcc.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Map as Map

-- | Calculate how much `Coin` exists on each `Maybe RewardAccount` in the
-- `UTxO` given a way to extract `Maybe RewardAccount` from an `Address`.
--
-- This is intended to be used with `rewardAccountFromAddress`, which exists
-- elsewhere because of the bcc-wallet-core / bcc-wallet split.
stakeKeyCoinDistr
    :: (Address -> Maybe RewardAccount)
    -> UTxO
    -> Map (Maybe RewardAccount) Coin
stakeKeyCoinDistr stakeRef =
    Map.fromListWith (<>) . map classifyOut . Map.elems . unUTxO
  where
    classifyOut :: TxOut -> (Maybe RewardAccount, Coin)
    classifyOut (TxOut addr b) = (stakeRef addr, TokenBundle.getCoin b)
