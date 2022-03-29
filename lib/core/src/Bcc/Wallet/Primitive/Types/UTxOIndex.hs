-- |
-- Copyright: © 2021-2022 TBCO
-- License: Apache-2.0
--
-- Provides the 'UTxOIndex' type, which indexes a UTxO set by asset ID.
--
-- The index makes it possible to efficiently compute the subset of a UTxO
-- set containing a particular asset, or to select a UTxO entry containing
-- that asset, without having to search through the entire UTxO set.
--
-- See the documentation for 'UTxOIndex' for more details.
--
-- This module is meant to be imported qualified. For example:
--
-- >>> import qualified Bcc.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
--
module Bcc.Wallet.Primitive.Types.UTxOIndex
    (
    -- * Type
      UTxOIndex

    -- * Construction
    , empty
    , singleton
    , fromSequence
    , fromUTxO

    -- * Deconstruction
    , toList
    , toUTxO

    -- * Folding
    , fold

    -- * Modification
    , insert
    , insertMany
    , delete
    , deleteMany

    -- * Filtering and partitioning
    , filter
    , partition

    -- * Queries
    , assets
    , balance
    , lookup
    , member
    , null
    , size

    -- * Set operations
    , difference
    , disjoint

    -- * Selection
    , SelectionFilter (..)
    , selectRandom
    , selectRandomWithPriority

    ) where

import Bcc.Wallet.Primitive.Types.UTxOIndex.Internal
