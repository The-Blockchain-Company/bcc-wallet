{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
-- Copyright: © 2021 TBCO
-- License: Apache-2.0
--
-- This module provides a public API for planning wallet migrations.
--
-- Use 'createPlan' to create a migration plan.
--
module Bcc.Wallet.Primitive.Migration
    (
    -- * Creating a migration plan
      createPlan
    , MigrationPlan (..)
    , RewardWithdrawal (..)
    , Selection (..)

    ) where

import Prelude

import Bcc.Wallet.Primitive.Migration.Selection
    ( RewardWithdrawal (..), Selection (..) )
import Bcc.Wallet.Primitive.Types.Coin
    ( Coin )
import Bcc.Wallet.Primitive.Types.Tx
    ( TxConstraints (..), TxIn, TxOut )
import Bcc.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import GHC.Generics
    ( Generic )

import qualified Bcc.Wallet.Primitive.Migration.Planning as Planning

-- | Represents a plan for migrating a 'UTxO' set.
--
-- See 'createPlan' to create a migration plan.
--
data MigrationPlan = MigrationPlan
    { selections :: ![Selection (TxIn, TxOut)]
      -- ^ A list of generated selections: each selection is the basis for a
      -- single transaction.
    , unselected :: !UTxO
      -- ^ The portion of the UTxO that was not selected.
    , totalFee :: !Coin
      -- ^ The total fee payable: equal to the sum of the fees of the
      -- individual selections.
    }
    deriving (Eq, Generic, Show)

-- | Creates a migration plan for the given UTxO set and reward withdrawal
--   amount.
--
-- See 'MigrationPlan'.
--
createPlan
    :: TxConstraints
    -> UTxO
    -> RewardWithdrawal
    -> MigrationPlan
createPlan constraints utxo reward = MigrationPlan
    { selections = view #selections plan
    , unselected = Planning.uncategorizeUTxO (view #unselected plan)
    , totalFee = view #totalFee plan
    }
  where
    plan = Planning.createPlan
        constraints (Planning.categorizeUTxO constraints utxo) reward
