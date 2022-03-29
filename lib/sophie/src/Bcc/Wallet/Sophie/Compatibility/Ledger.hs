{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2021 TBCO
-- License: Apache-2.0
--
-- Exposes a wallet-friendly interface to types and functions exported by the
-- ledger specification.
--
module Bcc.Wallet.Sophie.Compatibility.Ledger
    (
      -- * Exported ledger functions
      computeMinimumBccQuantity

      -- * Conversions from wallet types to ledger specification types
    , toLedgerCoin
    , toLedgerTokenBundle
    , toLedgerTokenPolicyId
    , toLedgerTokenName
    , toLedgerTokenQuantity
    , toAurumTxOut

      -- * Conversions from ledger specification types to wallet types
    , toWalletCoin
    , toWalletTokenBundle
    , toWalletTokenPolicyId
    , toWalletTokenName
    , toWalletTokenQuantity

      -- * Roundtrip conversion between wallet types and ledger specification
      --   types
    , Convert (..)

      -- * Internal functions
    , computeMinimumBccQuantityInternal

    ) where

import Prelude

import Bcc.Crypto.Hash
    ( hashFromBytes, hashToBytes )
import Bcc.Wallet.Primitive.Types
    ( MinimumUTxOValue (..) )
import Bcc.Wallet.Primitive.Types.Address
    ( Address (..) )
import Bcc.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Bcc.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Bcc.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Bcc.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Bcc.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Bcc.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Bcc.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Stack
    ( HasCallStack )
import Shardagnostic.Consensus.Sophie.Eras
    ( StandardCrypto )

import qualified Bcc.Ledger.Address as Ledger

import qualified Bcc.Ledger.Aurum as Aurum
import qualified Bcc.Ledger.Aurum.Rules.Utxo as Aurum
import qualified Bcc.Ledger.Aurum.TxBody as Aurum
import qualified Bcc.Ledger.Jen.Value as Ledger
import qualified Bcc.Ledger.SophieMA.Rules.Utxo as Ledger
import qualified Bcc.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Bcc.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap
import qualified Sophie.Spec.Ledger.API as Ledger

--------------------------------------------------------------------------------
-- Public functions
--------------------------------------------------------------------------------

-- | Uses the ledger specification to compute the minimum required bcc quantity
--   for a token map.
--
computeMinimumBccQuantity
    :: MinimumUTxOValue
    -- ^ The absolute minimum bcc quantity specified by the protocol.
    -> TokenMap
    -- ^ The token map to evaluate.
    -> Coin
    -- ^ The minimum bcc quantity for the given token map.
computeMinimumBccQuantity protocolMinimum m =
    -- Note:
    --
    -- We assume here that 'computeMinimumBccQuantityInternal' has the property
    -- of being constant w.r.t. to the bcc value. Assuming this property holds,
    -- it is safe to call it with an bcc value of 0.
    --
    -- See 'prop_computeMinimumBccQuantity_agnosticToBccQuantity'.
    --
    computeMinimumBccQuantityInternal protocolMinimum (TokenBundle (Coin 0) m)

--------------------------------------------------------------------------------
-- Roundtrip conversion between wallet types and ledger specification types
--------------------------------------------------------------------------------

-- | Connects a wallet type with its equivalent ledger specification type.
--
-- Instances of this class should satisfy the following laws:
--
-- >>> toLedger . toWallet == id
-- >>> toWallet . toLedger == id
--
class Convert wallet ledger | wallet -> ledger where
    -- | Converts a value from a wallet type to the equivalent ledger
    --   specification type.
    toLedger
        :: HasCallStack => wallet -> ledger
    -- | Converts a value from a ledger specification type to the equivalent
    --   wallet type.
    toWallet
        :: HasCallStack => ledger -> wallet

--------------------------------------------------------------------------------
-- Conversions for 'Coin'
--------------------------------------------------------------------------------

instance Convert Coin Ledger.Coin where
    toLedger = toLedgerCoin
    toWallet = toWalletCoin

toLedgerCoin :: Coin -> Ledger.Coin
toLedgerCoin (Coin c) =
      Ledger.Coin $ fromIntegral @Word64 @Integer c

toWalletCoin :: Ledger.Coin -> Coin
toWalletCoin (Ledger.Coin c)
    | isValidCoin =
        Coin $ fromIntegral @Integer @Word64 c
    | otherwise =
        error $ unwords
            [ "Ledger.toWalletCoin:"
            , "Unexpected invalid coin value:"
            , pretty c
            ]
  where
    isValidCoin = (&&)
        (c >= fromIntegral @Word64 @Integer (unCoin minBound))
        (c <= fromIntegral @Word64 @Integer (unCoin maxBound))

--------------------------------------------------------------------------------
-- Conversions for 'TokenBundle'
--------------------------------------------------------------------------------

-- Values of the ledger specification's 'Value' type are constructed in a way
-- that is similar to the wallet's 'TokenBundle' type. The bcc quantity is
-- stored as a separate value, and asset quantities are stored in a nested map.

instance Convert TokenBundle (Ledger.Value StandardCrypto) where
    toLedger = toLedgerTokenBundle
    toWallet = toWalletTokenBundle

toLedgerTokenBundle :: TokenBundle -> Ledger.Value StandardCrypto
toLedgerTokenBundle bundle =
    Ledger.Value ledgerBcc ledgerTokens
  where
    (Ledger.Coin ledgerBcc) = toLedgerCoin $ TokenBundle.getCoin bundle
    ledgerTokens = bundle
        & view #tokens
        & TokenMap.toNestedMap
        & Map.mapKeys toLedgerTokenPolicyId
        & Map.map mapInner
    mapInner inner = inner
        & NonEmptyMap.toMap
        & Map.mapKeys toLedgerTokenName
        & Map.map toLedgerTokenQuantity

toWalletTokenBundle :: Ledger.Value StandardCrypto -> TokenBundle
toWalletTokenBundle (Ledger.Value ledgerBcc ledgerTokens) =
    TokenBundle.fromNestedMap (walletBcc, walletTokens)
  where
    walletBcc = toWalletCoin $ Ledger.Coin ledgerBcc
    walletTokens = ledgerTokens
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
        & Map.mapMaybe NonEmptyMap.fromMap
    mapInner inner = inner
        & Map.mapKeys toWalletTokenName
        & Map.map toWalletTokenQuantity

--------------------------------------------------------------------------------
-- Conversions for 'TokenName'
--------------------------------------------------------------------------------

instance Convert TokenName Ledger.AssetName where
    toLedger = toLedgerTokenName
    toWallet = toWalletTokenName

toLedgerTokenName :: TokenName -> Ledger.AssetName
toLedgerTokenName (UnsafeTokenName bytes) =
    Ledger.AssetName bytes

toWalletTokenName :: Ledger.AssetName -> TokenName
toWalletTokenName (Ledger.AssetName bytes) =
    UnsafeTokenName bytes

--------------------------------------------------------------------------------
-- Conversions for 'TokenPolicyId'
--------------------------------------------------------------------------------

instance Convert TokenPolicyId (Ledger.PolicyID StandardCrypto) where
    toLedger = toLedgerTokenPolicyId
    toWallet = toWalletTokenPolicyId

toLedgerTokenPolicyId :: TokenPolicyId -> Ledger.PolicyID StandardCrypto
toLedgerTokenPolicyId p@(UnsafeTokenPolicyId (Hash bytes)) =
    case hashFromBytes bytes of
        Just hash ->
            Ledger.PolicyID (Ledger.ScriptHash hash)
        Nothing ->
            error $ unwords
                [ "Ledger.toLedgerTokenPolicyId"
                , "Unable to construct hash for token policy:"
                , pretty p
                ]

toWalletTokenPolicyId :: Ledger.PolicyID StandardCrypto -> TokenPolicyId
toWalletTokenPolicyId (Ledger.PolicyID (Ledger.ScriptHash hash)) =
    UnsafeTokenPolicyId (Hash (hashToBytes hash))

--------------------------------------------------------------------------------
-- Conversions for 'TokenQuantity'
--------------------------------------------------------------------------------

instance Convert TokenQuantity Integer where
    toLedger = toLedgerTokenQuantity
    toWallet = toWalletTokenQuantity

toLedgerTokenQuantity :: TokenQuantity -> Integer
toLedgerTokenQuantity (TokenQuantity q) = fromIntegral q

toWalletTokenQuantity :: Integer -> TokenQuantity
toWalletTokenQuantity q
    | q >= 0 =
        TokenQuantity $ fromIntegral q
    | otherwise =
        error $ unwords
            [ "Ledger.toWalletTokenQuantity:"
            , "Unexpected negative value:"
            , pretty q
            ]

--------------------------------------------------------------------------------
-- Conversions for 'Address'
--------------------------------------------------------------------------------

instance Convert Address (Ledger.Addr StandardCrypto) where
    toLedger (Address bytes ) = case Ledger.deserialiseAddr bytes of
        Just addr -> addr
        Nothing -> error $ unwords
            [ "toLedger @Address: Invalid address:"
            , pretty (Address bytes)
            ]
    toWallet = Address . Ledger.serialiseAddr

toAurumTxOut
    :: TxOut
    -> Aurum.TxOut (Aurum.AurumEra StandardCrypto)
toAurumTxOut (TxOut addr bundle) =
    Aurum.TxOut
        (toLedger addr)
        (toLedger bundle)
        Ledger.SNothing

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

-- | Uses the ledger specification to compute the minimum required bcc quantity
--   for a token bundle.
--
-- This function is intended to be constant with respect to:
--
--    - the bcc quantity;
--    - the quantities of individual assets.
--
-- See the following properties:
--
--    - 'prop_computeMinimumBccQuantity_agnosticToBccQuantity';
--    - 'prop_computeMinimumBccQuantity_agnosticToAssetQuantities'.
--
-- TODO: [ADP-954] Datum hashes are currently not taken into account.
computeMinimumBccQuantityInternal
    :: MinimumUTxOValue
    -- ^ The absolute minimum bcc quantity specified by the protocol.
    -> TokenBundle
    -- ^ The token bundle to evaluate.
    -> Coin
    -- ^ The minimum bcc quantity for the given token bundle.
computeMinimumBccQuantityInternal (MinimumUTxOValue protocolMinimum) bundle =
    toWalletCoin $
        Ledger.scaledMinDeposit
            (toLedgerTokenBundle bundle)
            (toLedgerCoin protocolMinimum)
computeMinimumBccQuantityInternal (MinimumUTxOValueCostPerWord (Coin perWord)) bundle =
    let
        outputSize = Aurum.utxoEntrySize (toAurumTxOut (TxOut dummyAddr bundle))
    in
        Coin $ fromIntegral outputSize * perWord
  where
    -- We just need an address the ledger can deserialize. It doesn't actually
    -- use the length of it.
    --
    -- This should not change (if Aurum is already in-use, it would have to be
    -- changed in a new era).
    --
    -- Regardless, the dummy address is a payment / enterprise address -- can't
    -- get any shorter than that. The integration tests use longer addresses.
    -- They should break if this were to be wrong.
    --
    -- Because the ledger function is pure and not taking a network, passing in
    -- a mainnet network should be fine regardless of network.
    dummyAddr = Address $ BS.pack $ 97 : replicate 28 0

