{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- Orphan instances for {Encode,Decode}Address until we get rid of the
-- Quibitous dual support.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2021 TBCO
-- License: Apache-2.0
--
-- Conversion functions and static chain settings for Sophie.

module Bcc.Wallet.Sophie.Compatibility
    ( BccBlock
    , StandardCrypto
    , StandardSophie

      -- * Protocol Parameters
    , NetworkId (..)
    , NodeToClientVersionData
    , nodeToClientVersions

      -- * Node Connection
    , localNodeConnectInfo

      -- * Genesis
    , emptyGenesis

      -- * Eras
    , AnyBccEra (..)
    , AnySophieBasedEra (..)
    , BccEra (..)
    , SophieBasedEra (..)
    , sophieBasedToBccEra
    , sophieToBccEra
    , getSophieBasedEra

      -- * Conversions
    , toBccHash
    , unsealSophieTx
    , toPoint
    , toBccTxId
    , toBccTxIn
    , toBccTxOut
    , toBccEntropic
    , toStakeKeyRegCert
    , toStakeKeyDeregCert
    , toStakePoolDlgCert
    , toStakeCredential
    , fromStakeCredential
    , toSophieCoin
    , fromSophieCoin
    , toHDPayloadAddress
    , toBccStakeCredential
    , toBccValue
    , fromBccValue
    , rewardAccountFromAddress
    , fromSophiePParams
    , fromAurumPParams
    , fromLedgerExUnits

      -- ** Assessing sizes of token bundles
    , tokenBundleSizeAssessor
    , computeTokenBundleSerializedLengthBytes

      -- ** Stake pools
    , fromPoolId
    , fromPoolDistr
    , fromNonMyopicMemberRewards
    , optimumNumberOfPools
    , getProducer

    , HasNetworkId (..)
    , fromBlockNo
    , fromBccBlock
    , toBccEra
    , toBccBlockHeader
    , toSophieBlockHeader
    , fromSophieHash
    , fromBccHash
    , fromChainHash
    , fromPrevHash
    , fromGenesisData
    , fromTip
    , fromTip'
    , fromBccTx
    , fromSophieTx
    , fromEvieTx
    , fromSophieBlock
    , fromEvieBlock
    , slottingParametersFromGenesis
    , fromJenBlock
    , fromJenTx
    , fromAurumTx
    , fromAurumBlock

      -- * Internal Conversions
    , decentralizationLevelFromPParams

      -- * Utilities
    , inspectAddress
    , invertUnitInterval
    , interval0
    , interval1
    ) where

import Prelude

import Bcc.Address
    ( unsafeMkAddress )
import Bcc.Address.Derivation
    ( XPub, xpubPublicKey )
import Bcc.Api
    ( EvieEra
    , AurumEra
    , AnyBccEra (..)
    , AsType (..)
    , BccEra (..)
    , BccEraStyle (..)
    , BccMode
    , ConsensusModeParams (BccModeParams)
    , EraInMode (..)
    , InAnyBccEra (..)
    , IsBccEra (..)
    , LocalNodeConnectInfo (LocalNodeConnectInfo)
    , JenEra
    , NetworkId
    , SophieEra
    , TxInMode (..)
    , bccEraStyle
    , deserialiseFromRawBytes
    )
import Bcc.Api.Sophie
    ( InAnySophieBasedEra (..)
    , IsSophieBasedEra (..)
    , SophieBasedEra (..)
    , SophieGenesis (..)
    , fromSophieMetadata
    )
import Bcc.Crypto.Hash.Class
    ( Hash (UnsafeHash), hashToBytes )
import Bcc.Launcher.Node
    ( BccNodeConn, nodeSocketFile )
import Bcc.Ledger.BaseTypes
    ( strictMaybeToMaybe, urlToText )
import Bcc.Ledger.Era
    ( Era (..) )
import Bcc.Ledger.Serialization
    ( ToCBORGroup )
import Bcc.Slotting.Slot
    ( EpochNo (..), EpochSize (..) )
import Bcc.Wallet.Api.Types
    ( DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , EncodeStakeAddress (..)
    )
import Bcc.Wallet.Cole.Compatibility
    ( fromColeBlock, fromTxAux, jenTokenBundleMaxSize, toColeBlockHeader )
import Bcc.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Bcc.Wallet.Primitive.Types
    ( MinimumUTxOValue (..)
    , PoolCertificate (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , ProtocolParameters (txParameters)
    , TxParameters (getTokenBundleMaxSize)
    )
import Bcc.Wallet.Unsafe
    ( unsafeIntToWord, unsafeMkPercentage )
import Bcc.Wallet.Util
    ( internalError, tina )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Control.Applicative
    ( Const (..), (<|>) )
import Control.Arrow
    ( left )
import Control.Monad
    ( when, (>=>) )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.Bifunctor
    ( bimap )
import Data.Binary.Get
    ( runGetOrFail )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( (.&.), (.|.) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.ByteString.Short
    ( fromShort, toShort )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( toList )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust, mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..), mkPercentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Tuple.Extra
    ( fst3 )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import Fmt
    ( Buildable (..), Builder, (+|), (+||), (||+) )
import GHC.Records
    ( HasField (..) )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat, natVal )
import Numeric.Natural
    ( Natural )
import Shardagnostic.Consensus.Bcc.Block
    ( BccBlock
    , BccEras
    , HardForkBlock (..)
    , StandardAurum
    , StandardSophie
    )
import Shardagnostic.Consensus.HardFork.Combinator.AcrossEras
    ( OneEraHash (..) )
import Shardagnostic.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Shardagnostic.Consensus.Sophie.Eras
    ( StandardCrypto )
import Shardagnostic.Consensus.Sophie.Ledger
    ( SophieHash (..) )
import Shardagnostic.Consensus.Sophie.Ledger.Block
    ( SophieBlock (..) )
import Shardagnostic.Network.Block
    ( BlockNo (..), ChainHash, Point (..), Tip (..), getTipPoint )
import Shardagnostic.Network.NodeToClient
    ( ConnectionId (..)
    , LocalAddress (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData
    )
import Shardagnostic.Network.Point
    ( WithOrigin (..) )

import qualified Bcc.Address.Style.Sophie as CA
import qualified Bcc.Api as Bcc
import qualified Bcc.Api.Cole as Bcc
    ( Tx (ColeTx) )
import qualified Bcc.Api.Sophie as Bcc
import qualified Bcc.Binary as Binary
import qualified Bcc.Cole.Codec.Cbor as CBOR
import qualified Bcc.Chain.Common as Cole
import qualified Bcc.Crypto.Hash as Crypto
import qualified Bcc.Ledger.Address as SL
import qualified Bcc.Ledger.Aurum as Aurum
import qualified Bcc.Ledger.Aurum.Data as Aurum
import qualified Bcc.Ledger.Aurum.PParams as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import qualified Bcc.Ledger.Aurum.TxBody as Aurum
import qualified Bcc.Ledger.Aurum.TxSeq as Aurum
import qualified Bcc.Ledger.BaseTypes as Ledger
import qualified Bcc.Ledger.BaseTypes as SL
import qualified Bcc.Ledger.Core as SL.Core
import qualified Bcc.Ledger.Credential as SL
import qualified Bcc.Ledger.Crypto as SL
import qualified Bcc.Ledger.Era as Ledger.Era
import qualified Bcc.Ledger.SafeHash as SafeHash
import qualified Bcc.Ledger.Sophie as SL
import qualified Bcc.Ledger.Sophie.Constraints as SL
import qualified Bcc.Ledger.SophieMA as MA
import qualified Bcc.Ledger.SophieMA.AuxiliaryData as MA
import qualified Bcc.Ledger.SophieMA.TxBody as MA
import qualified Bcc.Wallet.Primitive.Types as W
import qualified Bcc.Wallet.Primitive.Types.Address as W
import qualified Bcc.Wallet.Primitive.Types.Coin as W
import qualified Bcc.Wallet.Primitive.Types.Hash as W
import qualified Bcc.Wallet.Primitive.Types.RewardAccount as W
import qualified Bcc.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Bcc.Wallet.Primitive.Types.TokenPolicy as W
import qualified Bcc.Wallet.Primitive.Types.TokenQuantity as W
import qualified Bcc.Wallet.Primitive.Types.Tx as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Codec.CBOR.Decoding as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import qualified Shardagnostic.Consensus.Sophie.Ledger as O
import qualified Shardagnostic.Network.Block as O
import qualified Shardagnostic.Network.Point as Point
import qualified Sophie.Spec.Ledger.API as SL
import qualified Sophie.Spec.Ledger.API as SLAPI
import qualified Sophie.Spec.Ledger.BlockChain as SL
import qualified Sophie.Spec.Ledger.UTxO as SL

--------------------------------------------------------------------------------
--
-- Chain Parameters

-- NOTE
-- For MainNet and TestNet, we can get away with empty genesis blocks with
-- the following assumption:
--
-- - Users won't ever restore a wallet that has genesis UTxO.
--
-- This assumption is _true_ for any user using HD wallets (sequential or
-- random) which means, any user of bcc-wallet.
emptyGenesis :: W.GenesisParameters -> W.Block
emptyGenesis gp = W.Block
    { transactions = []
    , delegations  = []
    , header = W.BlockHeader
        { slotNo =
            W.SlotNo 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ W.getGenesisBlockHash gp
        , parentHeaderHash =
            hashOfNoParent
        }
    }

--------------------------------------------------------------------------------
--
-- Network Parameters

-- | The protocol client version. Distinct from the codecs version.
nodeToClientVersions :: [NodeToClientVersion]
nodeToClientVersions = [NodeToClientV_8, NodeToClientV_9]

--------------------------------------------------------------------------------
--
-- Type Conversions

-- | Magic value for the absence of a block.
hashOfNoParent :: W.Hash "BlockHeader"
hashOfNoParent =
    W.Hash . BS.pack $ replicate 32 0

toBccHash :: W.Hash "BlockHeader" -> OneEraHash (BccEras sc)
toBccHash (W.Hash bytes) =
    OneEraHash $ toShort bytes

toPoint
    :: W.Hash "Genesis"
    -> W.BlockHeader
    -> Point (BccBlock sc)
toPoint genesisH (W.BlockHeader sl _ (W.Hash h) _)
  | h == (coerce genesisH) = O.GenesisPoint
  | otherwise = O.BlockPoint sl (OneEraHash $ toShort h)

toBccBlockHeader
    :: forall c. Era (SL.SophieEra c)
    => W.GenesisParameters
    -> BccBlock c
    -> W.BlockHeader
toBccBlockHeader gp = \case
    BlockCole blk ->
        toColeBlockHeader gp blk
    BlockSophie blk ->
        toSophieBlockHeader (W.getGenesisBlockHash gp) blk
    BlockEvie blk ->
        toSophieBlockHeader (W.getGenesisBlockHash gp) blk
    BlockJen blk ->
        toSophieBlockHeader (W.getGenesisBlockHash gp) blk
    BlockAurum blk ->
        toSophieBlockHeader (W.getGenesisBlockHash gp) blk

toSophieBlockHeader
    :: (Era e, ToCBORGroup (Ledger.Era.TxSeq e))
    => W.Hash "Genesis"
    -> SophieBlock e
    -> W.BlockHeader
toSophieBlockHeader genesisHash blk =
    let
        SophieBlock (SL.Block (SL.BHeader header _) _) headerHash = blk
    in
        W.BlockHeader
            { slotNo =
                SL.bheaderSlotNo header
            , blockHeight =
                fromBlockNo $ SL.bheaderBlockNo header
            , headerHash =
                fromSophieHash headerHash
            , parentHeaderHash =
                fromPrevHash (coerce genesisHash) $
                    SL.bheaderPrev header
            }

getProducer
    :: (Era e, ToCBORGroup (Ledger.Era.TxSeq e))
    => SophieBlock e -> W.PoolId
getProducer (SophieBlock (SL.Block (SL.BHeader header _) _) _) =
    fromPoolKeyHash $ SL.hashKey (SL.bheaderVk header)

fromBccBlock
    :: W.GenesisParameters
    -> BccBlock StandardCrypto
    -> W.Block
fromBccBlock gp = \case
    BlockCole blk ->
        fromColeBlock gp blk
    BlockSophie blk ->
        fst $ fromSophieBlock gp blk
    BlockEvie blk ->
        fst $ fromEvieBlock gp blk
    BlockJen blk ->
        fst $ fromJenBlock gp blk
    BlockAurum blk ->
        fst $ fromAurumBlock gp blk

toBccEra :: BccBlock c -> AnyBccEra
toBccEra = \case
    BlockCole{}   -> AnyBccEra ColeEra
    BlockSophie{} -> AnyBccEra SophieEra
    BlockEvie{} -> AnyBccEra EvieEra
    BlockJen{}    -> AnyBccEra JenEra
    BlockAurum{}  -> AnyBccEra AurumEra

fromSophieBlock
    :: W.GenesisParameters
    -> SophieBlock (SL.SophieEra StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromSophieBlock gp blk@(SophieBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, dlgCerts, poolCerts) = unzip3 $ map fromSophieTx $ toList txs'
    in
        ( W.Block
            { header = toSophieBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )

fromEvieBlock
    :: W.GenesisParameters
    -> SophieBlock (MA.SophieMAEra 'MA.Evie StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromEvieBlock gp blk@(SophieBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, dlgCerts, poolCerts) = unzip3 $ map fromEvieTx $ toList txs'
    in
        ( W.Block
            { header = toSophieBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )


fromJenBlock
    :: W.GenesisParameters
    -> SophieBlock (MA.SophieMAEra 'MA.Jen StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromJenBlock gp blk@(SophieBlock (SL.Block _ (SL.TxSeq txs')) _) =
    let
       (txs, dlgCerts, poolCerts) = unzip3 $ map fromJenTx $ toList txs'
    in
        ( W.Block
            { header = toSophieBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )

-- TODO: We could use the bcc-api `Block` pattern to very elegently get the
-- header and txs of any era block.
--
-- We would need to remove the previous block hash from our `W.BlockHeader`,
-- which shouldn't be needed modulo some hacks w.r.t. the genesis point which
-- would need to be cleaned up too. We probably will need to use `Point block`,
-- in all chain followers (including the DBLayer).
fromAurumBlock
    :: W.GenesisParameters
    -> SophieBlock (Aurum.AurumEra StandardCrypto)
    -> (W.Block, [W.PoolCertificate])
fromAurumBlock gp blk@(SophieBlock (SL.Block _ txSeq) _) =
    let
        Aurum.TxSeq txs' = txSeq
        (txs, dlgCerts, poolCerts) = unzip3 $ map fromAurumValidatedTx $ toList txs'
    in
        ( W.Block
            { header = toSophieBlockHeader (W.getGenesisBlockHash gp) blk
            , transactions = txs
            , delegations  = mconcat dlgCerts
            }
        , mconcat poolCerts
        )

fromSophieHash :: SophieHash c -> W.Hash "BlockHeader"
fromSophieHash (SophieHash (SL.HashHeader h)) = W.Hash (hashToBytes h)

fromBccHash :: O.HeaderHash (BccBlock sc) -> W.Hash "BlockHeader"
fromBccHash = W.Hash . fromShort . getOneEraHash

fromPrevHash
    :: W.Hash "BlockHeader"
    -> SLAPI.PrevHash sc
    -> W.Hash "BlockHeader"
fromPrevHash genesisHash = \case
    SL.GenesisHash -> genesisHash
    SL.BlockHash h -> fromSophieHash (SophieHash h)

fromChainHash
    :: W.Hash "Genesis"
    -> ChainHash (BccBlock sc)
    -> W.Hash "BlockHeader"
fromChainHash genesisHash = \case
    O.GenesisHash -> coerce genesisHash
    O.BlockHash (OneEraHash h) -> W.Hash $ fromShort h

-- FIXME unsafe conversion (Word64 -> Word32)
fromBlockNo :: BlockNo -> Quantity "block" Word32
fromBlockNo (BlockNo h) =
    Quantity (fromIntegral h)

fromTip' :: W.GenesisParameters -> Tip (BccBlock sc) -> W.BlockHeader
fromTip' gp = fromTip (W.getGenesisBlockHash gp)

fromTip
    :: W.Hash "Genesis"
    -> Tip (BccBlock sc)
    -> W.BlockHeader
fromTip genesisHash tip = case getPoint (getTipPoint tip) of
    Origin -> W.BlockHeader
        { slotNo = W.SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = coerce genesisHash
        , parentHeaderHash = hashOfNoParent
        }
    At blk -> W.BlockHeader
        { slotNo = Point.blockPointSlot blk
        , blockHeight = fromBlockNo $ getLegacyTipBlockNo tip
        , headerHash = fromBccHash $ Point.blockPointHash blk
        -- TODO: parentHeaderHash could be removed.
        , parentHeaderHash = W.Hash "parentHeaderHash - unused in Sophie"
        }
  where
    -- TODO: This function was marked deprecated in shardagnostic-network.
    -- It is wrong, because `Origin` doesn't have a block number.
    -- We should remove it.
    getLegacyTipBlockNo t = case O.getTipBlockNo t of
        Origin -> BlockNo 0
        At x -> x

-- NOTE: Unsafe conversion from Natural -> Word16
fromMaxSize :: Natural -> Quantity "byte" Word16
fromMaxSize =
    Quantity . fromIntegral

fromSophiePParams
    :: HasCallStack
    => W.EraInfo Bound
    -> SLAPI.PParams era
    -> W.ProtocolParameters
fromSophiePParams eraInfo pp = W.ProtocolParameters
    { decentralizationLevel =
        decentralizationLevelFromPParams pp
    , txParameters =
        txParametersFromPParams jenTokenBundleMaxSize pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue =
        MinimumUTxOValue . toWalletCoin $ SLAPI._minUTxOValue pp
    , stakeKeyDeposit = stakeKeyDepositFromPParams pp
    , eras = fromBound <$> eraInfo
    , maximumCollateralInputCount = minBound
    , executionUnitPrices = Nothing
    }
  where
    fromBound (Bound _relTime _slotNo (EpochNo e)) =
        W.EpochNo $ fromIntegral e

fromAurumPParams
    :: HasCallStack
    => W.EraInfo Bound
    -> Aurum.PParams StandardAurum
    -> W.ProtocolParameters
fromAurumPParams eraInfo pp = W.ProtocolParameters
    { decentralizationLevel =
        decentralizationLevelFromPParams pp
    , txParameters = txParametersFromPParams
        (W.TokenBundleMaxSize $ W.TxSize $ Aurum._maxValSize pp)
        pp
    , desiredNumberOfStakePools =
        desiredNumberOfStakePoolsFromPParams pp
    , minimumUTxOvalue = MinimumUTxOValueCostPerWord
        . toWalletCoin $ Aurum._coinsPerUTxOWord pp
    , stakeKeyDeposit = stakeKeyDepositFromPParams pp
    , eras = fromBound <$> eraInfo
    , maximumCollateralInputCount = unsafeIntToWord $
        Aurum._maxCollateralInputs pp
    , executionUnitPrices =
        Just $ executionUnitPricesFromPParams pp
    }
  where
    fromBound (Bound _relTime _slotNo (EpochNo e)) =
        W.EpochNo $ fromIntegral e

-- | Extract the current network decentralization level from the given set of
-- protocol parameters.
--
-- According to the Design Specification for Delegation and Incentives in
-- Bcc, the decentralization parameter __/d/__ is a value in the range
-- '[0, 1]', where:
--
--   * __/d/__ = '1' indicates that the network is /completely federalized/.
--   * __/d/__ = '0' indicates that the network is /completely decentralized/.
--
-- However, in Bcc Wallet, we represent the decentralization level as a
-- percentage, where:
--
--   * '  0 %' indicates that the network is /completely federalized/.
--   * '100 %' indicates that the network is /completely decentralized/.
--
-- Therefore, we must invert the value provided by bcc-node before we
-- convert it into a percentage.
--
decentralizationLevelFromPParams
    :: HasField "_d" pparams SL.UnitInterval
    => pparams
    -> W.DecentralizationLevel
decentralizationLevelFromPParams pp =
    W.DecentralizationLevel $ fromUnitInterval
        -- We must invert the value provided: (see function comment)
        $ invertUnitInterval d
  where
    d = getField @"_d" pp

executionUnitPricesFromPParams
    :: HasField "_prices" pparams Aurum.Prices
    => pparams
    -> W.ExecutionUnitPrices
executionUnitPricesFromPParams pp =
    fromAurumPrices prices
  where
    prices = getField @"_prices" pp
    fromAurumPrices (Aurum.Prices prSteps prMem) =
        W.ExecutionUnitPrices
        { W.priceExecutionSteps  = Ledger.unboundRational prSteps
        , W.priceExecutionMemory = Ledger.unboundRational prMem
        }

fromLedgerExUnits
    :: Aurum.ExUnits
    -> W.ExecutionUnits
fromLedgerExUnits (Aurum.ExUnits mem steps) =
    W.ExecutionUnits
    { executionSteps = steps
    , executionMemory = mem
    }

txParametersFromPParams
    :: HasField "_minfeeA" pparams Natural
    => HasField "_minfeeB" pparams Natural
    => HasField "_maxTxSize" pparams Natural
    => W.TokenBundleMaxSize
    -> pparams
    -> W.TxParameters
txParametersFromPParams maxBundleSize pp = W.TxParameters
    { getFeePolicy = W.LinearFee
        (Quantity (naturalToDouble (getField @"_minfeeB" pp)))
        (Quantity (naturalToDouble (getField @"_minfeeA" pp)))
    , getTxMaxSize = fromMaxSize $ getField @"_maxTxSize" pp
    , getTokenBundleMaxSize = maxBundleSize
    }
  where
    naturalToDouble :: Natural -> Double
    naturalToDouble = fromIntegral

desiredNumberOfStakePoolsFromPParams
    :: HasField "_nOpt" pparams Natural
    => pparams
    -> Word16
desiredNumberOfStakePoolsFromPParams pp = fromIntegral $ getField @"_nOpt" pp

stakeKeyDepositFromPParams
    :: HasField "_keyDeposit" pparams SLAPI.Coin
    => pparams
    -> W.Coin
stakeKeyDepositFromPParams = toWalletCoin . getField @"_keyDeposit"

slottingParametersFromGenesis
    :: SophieGenesis e
    -> W.SlottingParameters
slottingParametersFromGenesis g =
    W.SlottingParameters
        { getSlotLength =
            W.SlotLength $ sgSlotLength g
        , getEpochLength =
            W.EpochLength . fromIntegral . unEpochSize . sgEpochLength $ g
        , getActiveSlotCoefficient =
            W.ActiveSlotCoefficient . fromRational . SL.unboundRational . sgActiveSlotsCoeff $ g
        , getSecurityParameter =
            Quantity . fromIntegral . sgSecurityParam $ g
        }

-- note: upcasts Word32 -> Word64
getBccEpochSlots :: W.SlottingParameters -> Bcc.EpochSlots
getBccEpochSlots =
    Bcc.EpochSlots . fromIntegral . W.unEpochLength . W.getEpochLength

localNodeConnectInfo
    :: W.SlottingParameters
    -> NetworkId
    -> BccNodeConn
    -> LocalNodeConnectInfo BccMode
localNodeConnectInfo sp net = LocalNodeConnectInfo params net . nodeSocketFile
    where params = BccModeParams (getBccEpochSlots sp)

-- | Convert genesis data into blockchain params and an initial set of UTxO
fromGenesisData
    :: forall e crypto. (Era e, e ~ SL.SophieEra crypto)
    => SophieGenesis e
    -> [(SL.Addr crypto, SL.Coin)]
    -> (W.NetworkParameters, W.Block)
fromGenesisData g initialFunds =
    ( W.NetworkParameters
        { genesisParameters = W.GenesisParameters
            { getGenesisBlockHash = dummyGenesisHash
            , getGenesisBlockDate =
                W.StartTime . sgSystemStart $ g
            }
        , slottingParameters =
            slottingParametersFromGenesis g
        , protocolParameters =
            (fromSophiePParams W.emptyEraInfo) . sgProtocolParams $ g
        }
    , genesisBlockFromTxOuts initialFunds
    )
  where
    -- TODO: There is not yet any agreed upon definition of a
    -- genesis hash for a sophie-only testnet.
    --
    -- For now we use a dummy value.
    dummyGenesisHash = W.Hash . BS.pack $ replicate 32 1


    -- | Construct a ("fake") genesis block from genesis transaction outputs.
    --
    -- The genesis data on haskell nodes is not a block at all, unlike the
    -- block0 on quibitous. This function is a method to deal with the
    -- discrepancy.
    genesisBlockFromTxOuts :: [(SL.Addr crypto, SL.Coin)] -> W.Block
    genesisBlockFromTxOuts outs = W.Block
        { delegations  = []
        , header = W.BlockHeader
            { slotNo =
                W.SlotNo 0
            , blockHeight =
                Quantity 0
            , headerHash =
                dummyGenesisHash
            , parentHeaderHash =
                W.Hash (BS.replicate 32 0)
            }
        , transactions = mkTx <$> outs
        }
      where
        mkTx (addr, c) = W.Tx
            { txId = pseudoHash
            , fee = Nothing
            , resolvedCollateral = []
            , resolvedInputs = []
            , outputs =
                [W.TxOut
                    (fromSophieAddress addr)
                    (TokenBundle.fromCoin $ fromSophieCoin c)
                ]
            , withdrawals = mempty
            , metadata = Nothing
            , scriptValidity = Nothing
            }
          where
            W.TxIn pseudoHash _ = fromSophieTxIn $
                SL.initialFundsPseudoTxIn @crypto addr

--
-- Stake pools
--

fromPoolId :: forall crypto. SL.KeyHash 'SL.StakePool crypto -> W.PoolId
fromPoolId (SL.KeyHash x) = W.PoolId $ hashToBytes x

fromPoolDistr
    :: forall crypto. ()
    => SL.PoolDistr crypto
    -> Map W.PoolId Percentage
fromPoolDistr =
    Map.map (unsafeMkPercentage . SL.individualPoolStake)
    . Map.mapKeys fromPoolId
    . SL.unPoolDistr

-- NOTE: This function disregards results that are using staking keys
fromNonMyopicMemberRewards
    :: forall era. ()
    => O.NonMyopicMemberRewards era
    -> Map (Either W.Coin W.RewardAccount) (Map W.PoolId W.Coin)
fromNonMyopicMemberRewards =
    Map.map (Map.map toWalletCoin . Map.mapKeys fromPoolId)
    . Map.mapKeys (bimap fromSophieCoin fromStakeCredential)
    . O.unNonMyopicMemberRewards

optimumNumberOfPools
    :: HasField "_nOpt" pparams Natural
    => pparams
    -> Int
optimumNumberOfPools = unsafeConvert . getField @"_nOpt"
  where
    -- A value of ~100 can be expected, so should be fine.
    unsafeConvert :: Natural -> Int
    unsafeConvert = fromIntegral

--
-- Txs
--

fromSophieTxId :: SL.TxId crypto -> W.Hash "Tx"
fromSophieTxId (SL.TxId h) =
    W.Hash $ Crypto.hashToBytes $ SafeHash.extractHash h

fromSophieTxIn
    :: SL.Crypto crypto
    => SL.TxIn crypto
    -> W.TxIn
fromSophieTxIn (SL.TxIn txid ix) =
    W.TxIn (fromSophieTxId txid) (unsafeCast ix)
  where
    unsafeCast :: Natural -> Word32
    unsafeCast = fromIntegral

fromSophieTxOut
    :: ( SL.SophieBased era
       , SL.Core.Value era ~ SL.Coin
       )
    => SLAPI.TxOut era
    -> W.TxOut
fromSophieTxOut (SLAPI.TxOut addr amount) = W.TxOut
    (fromSophieAddress addr)
    (TokenBundle.fromCoin $ fromSophieCoin amount)

fromSophieAddress :: SL.Addr crypto -> W.Address
fromSophieAddress = W.Address
    . SL.serialiseAddr

fromSophieCoin :: SL.Coin -> W.Coin
fromSophieCoin (SL.Coin c) = W.Coin $ unsafeCast c
  where
    -- (but probably safe)
    unsafeCast :: Integer -> Word64
    unsafeCast = fromIntegral

toSophieCoin :: W.Coin -> SL.Coin
toSophieCoin (W.Coin c) = SL.Coin $ safeCast c
  where
    safeCast :: Word64 -> Integer
    safeCast = fromIntegral

fromBccTx :: Bcc.Tx era -> W.Tx
fromBccTx = \case
    Bcc.SophieTx era tx -> case era of
        Bcc.SophieBasedEraSophie -> fst3 $ fromSophieTx tx
        Bcc.SophieBasedEraEvie -> fst3 $ fromEvieTx tx
        Bcc.SophieBasedEraJen    -> fst3 $ fromJenTx tx
        Bcc.SophieBasedEraAurum  -> fst3 $ fromAurumTx tx
    Bcc.ColeTx tx                 -> fromTxAux tx

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.
fromSophieTx
    :: SLAPI.Tx (Bcc.SophieLedgerEra SophieEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromSophieTx tx =
    ( W.Tx
        { txId =
            fromSophieTxId $ SL.txid @(Bcc.SophieLedgerEra SophieEra) bod
        , fee =
            Just $ fromSophieCoin fee
        , resolvedCollateral =
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromSophieTxIn) (toList ins)
        , outputs =
            map fromSophieTxOut (toList outs)
        , withdrawals =
            fromSophieWdrl wdrls
        , metadata =
            fromSophieMD <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromSophieDelegationCert (toList certs)
    , mapMaybe fromSophieRegistrationCert (toList certs)
    )
  where
    SL.Tx bod@(SL.TxBody ins outs certs wdrls fee _ _ _) _ mmd = tx

fromEvieTx
    :: SLAPI.Tx (Bcc.SophieLedgerEra EvieEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromEvieTx tx =
    ( W.Tx
        { txId =
            fromSophieTxId $ SL.txid @(Bcc.SophieLedgerEra EvieEra) bod
        , fee =
            Just $ fromSophieCoin fee
        , resolvedCollateral =
            -- TODO: (ADP-957)
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromSophieTxIn) (toList ins)
        , outputs =
            map fromSophieTxOut (toList outs)
        , withdrawals =
            fromSophieWdrl wdrls
        , metadata =
            fromSophieMD . toSLMetadata <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromSophieDelegationCert (toList certs)
    , mapMaybe fromSophieRegistrationCert (toList certs)
    )
  where
    SL.Tx bod@(MA.TxBody ins outs certs wdrls fee _ _ _ _) _ mmd = tx

    -- fixme: [ADP-525] It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.AuxiliaryData blob _scripts) = SL.Metadata blob

fromJenTx
    :: SLAPI.Tx (Bcc.SophieLedgerEra JenEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromJenTx tx =
    ( W.Tx
        { txId
            = fromSophieTxId $ SL.txid @(Bcc.SophieLedgerEra JenEra) bod
        , fee =
            Just $ fromSophieCoin fee
        , resolvedCollateral =
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromSophieTxIn) (toList ins)
        , outputs =
            map fromJenTxOut (toList outs)
        , withdrawals =
            fromSophieWdrl wdrls
        , metadata =
            fromSophieMD . toSLMetadata <$> SL.strictMaybeToMaybe mad
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromSophieDelegationCert (toList certs)
    , mapMaybe fromSophieRegistrationCert (toList certs)
    )
  where
    SL.Tx bod _wits mad = tx
    MA.TxBody ins outs certs wdrls fee _valid _upd _adh _value = bod

    -- fixme: [ADP-525] It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.AuxiliaryData blob _scripts) = SL.Metadata blob

    fromJenTxOut
         :: SLAPI.TxOut (Bcc.SophieLedgerEra JenEra)
         -> W.TxOut
    fromJenTxOut (SL.TxOut addr value) =
        W.TxOut (fromSophieAddress addr) $
        fromBccValue $ Bcc.fromJenValue value

fromAurumTxBodyAndAux
    :: Aurum.TxBody (Bcc.SophieLedgerEra AurumEra)
    -> SLAPI.StrictMaybe (Aurum.AuxiliaryData (Bcc.SophieLedgerEra AurumEra))
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAurumTxBodyAndAux bod mad =
    ( W.Tx
        { txId =
            fromSophieTxId $ SL.txid @(Bcc.SophieLedgerEra AurumEra) bod
        , fee =
            Just $ fromSophieCoin fee
        , resolvedCollateral =
            -- TODO: (ADP-957)
            []
        , resolvedInputs =
            map ((,W.Coin 0) . fromSophieTxIn) (toList ins)
        , outputs =
            map fromAurumTxOut (toList outs)
        , withdrawals =
            fromSophieWdrl wdrls
        , metadata =
            fromSophieMD . toSLMetadata <$> SL.strictMaybeToMaybe mad
        , scriptValidity =
            Nothing
        }
    , mapMaybe fromSophieDelegationCert (toList certs)
    , mapMaybe fromSophieRegistrationCert (toList certs)
    )
  where
    Aurum.TxBody
        ins
        _collateral
        outs
        certs
        wdrls
        fee
        _valid
        _upd
        _reqSignerHashes
        _mint
        _wwpHash
        _adHash
        _network
        = bod

    fromAurumTxOut
         :: Aurum.TxOut (Bcc.SophieLedgerEra AurumEra)
         -> W.TxOut
    fromAurumTxOut (Aurum.TxOut addr value _) =
        W.TxOut (fromSophieAddress addr) $
        fromBccValue $ Bcc.fromJenValue value

    toSLMetadata (Aurum.AuxiliaryData blob _scripts) = SL.Metadata blob

fromAurumValidatedTx
    :: Aurum.ValidatedTx (Bcc.SophieLedgerEra AurumEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAurumValidatedTx (Aurum.ValidatedTx bod _wits _isValidating aux) =
    fromAurumTxBodyAndAux bod aux

fromAurumTx
    :: Aurum.ValidatedTx (Bcc.SophieLedgerEra AurumEra)
    -> ( W.Tx
       , [W.DelegationCertificate]
       , [W.PoolCertificate]
       )
fromAurumTx (Aurum.ValidatedTx bod _wits (Aurum.IsValid isValid) aux) =
    (\(tx, d, p) -> (tx { W.scriptValidity = validity }, d, p))
    $ fromAurumTxBodyAndAux bod aux
    where
        validity =
            if isValid
            then Just W.TxScriptValid
            else Just W.TxScriptInvalid

fromBccValue :: HasCallStack => Bcc.Value -> TokenBundle.TokenBundle
fromBccValue = uncurry TokenBundle.fromFlatList . extract
  where
    extract value =
        ( mkCoin $ Bcc.selectEntropic value
        , mkBundle $ Bcc.valueToList value
        )

    -- Entropic to coin. Quantities from ledger should always fit in Word64.
    mkCoin :: Bcc.Entropic -> W.Coin
    mkCoin = W.Coin . unsafeIntToWord . unQuantity . Bcc.entropicToQuantity

    -- Do Integer to Natural conversion. Quantities from ledger TxOuts can
    -- never be negative (but unminted values could be negative).
    mkQuantity :: Integer -> W.TokenQuantity
    mkQuantity = W.TokenQuantity . checkBounds
      where
        checkBounds n
          | n >= 0 = fromIntegral n
          | otherwise = internalError "negative token quantity"

    mkBundle assets =
        [ (TokenBundle.AssetId (mkPolicyId p) (mkTokenName n) , mkQuantity q)
        | (Bcc.AssetId p n, Bcc.Quantity q) <- assets
        ]

    mkPolicyId = W.UnsafeTokenPolicyId . W.Hash . Bcc.serialiseToRawBytes
    mkTokenName = W.UnsafeTokenName . Bcc.serialiseToRawBytes

    unQuantity (Bcc.Quantity q) = q

fromSophieWdrl :: SL.Wdrl crypto -> Map W.RewardAccount W.Coin
fromSophieWdrl (SL.Wdrl wdrl) = Map.fromList $
    bimap (fromStakeCredential . SL.getRwdCred) fromSophieCoin
        <$> Map.toList wdrl

fromSophieMD :: SL.Metadata c -> Bcc.TxMetadata
fromSophieMD (SL.Metadata m) =
    Bcc.makeTransactionMetadata . fromSophieMetadata $ m

-- Convert & filter Sophie certificate into delegation certificate. Returns
-- 'Nothing' if certificates aren't delegation certificate.
fromSophieDelegationCert
    :: SL.DCert crypto
    -> Maybe W.DelegationCertificate
fromSophieDelegationCert = \case
    SL.DCertDeleg (SL.Delegate delegation)  ->
        Just $ W.CertDelegateFull
            (fromStakeCredential (SL._delegator delegation))
            (fromPoolKeyHash (SL._delegatee delegation))

    SL.DCertDeleg (SL.DeRegKey credentials) ->
        Just $ W.CertDelegateNone (fromStakeCredential credentials)

    SL.DCertDeleg (SL.RegKey cred) ->
        Just $ W.CertRegisterKey $ fromStakeCredential cred
    SL.DCertPool{}            -> Nothing
    SL.DCertGenesis{}         -> Nothing
    SL.DCertMir{}             -> Nothing

-- Convert & filter Sophie certificate into delegation certificate. Returns
-- 'Nothing' if certificates aren't delegation certificate.
fromSophieRegistrationCert
    :: SL.DCert crypto
    -> Maybe (W.PoolCertificate)
fromSophieRegistrationCert = \case
    SL.DCertPool (SL.RegPool pp) -> Just $ Registration
        ( W.PoolRegistrationCertificate
            { W.poolId = fromPoolKeyHash $ SL._poolId pp
            , W.poolOwners = fromOwnerKeyHash <$> Set.toList (SL._poolOwners pp)
            , W.poolMargin = fromUnitInterval (SL._poolMargin pp)
            , W.poolCost = toWalletCoin (SL._poolCost pp)
            , W.poolPledge = toWalletCoin (SL._poolPledge pp)
            , W.poolMetadata = fromPoolMetadata <$> strictMaybeToMaybe (SL._poolMD pp)
            }
        )

    SL.DCertPool (SL.RetirePool pid (EpochNo e)) ->
        Just $ Retirement $ PoolRetirementCertificate (fromPoolKeyHash pid)
        (W.EpochNo $ fromIntegral e)

    SL.DCertDeleg{}   -> Nothing
    SL.DCertGenesis{} -> Nothing
    SL.DCertMir{}     -> Nothing

toWalletCoin :: HasCallStack => SL.Coin -> W.Coin
toWalletCoin = W.Coin . unsafeCoinToWord64

-- | The reverse of 'word64ToCoin', where overflow is fatal.
unsafeCoinToWord64 :: HasCallStack => SL.Coin -> Word64
unsafeCoinToWord64 (SL.Coin c) = unsafeIntToWord c

fromPoolMetadata :: SL.PoolMetadata -> (W.StakePoolMetadataUrl, W.StakePoolMetadataHash)
fromPoolMetadata meta =
    ( W.StakePoolMetadataUrl (urlToText (SL._poolMDUrl meta))
    , W.StakePoolMetadataHash (SL._poolMDHash meta)
    )

-- | Convert a stake credentials to a 'RewardAccount' type.
--
-- Unlike with Quibitous, the reward account payload doesn't represent a
-- public key but a HASH of a public key.
--
fromStakeCredential :: SL.Credential 'SL.Staking crypto -> W.RewardAccount
fromStakeCredential = \case
    SL.ScriptHashObj (SL.ScriptHash h) ->
        W.RewardAccount (hashToBytes h)
    SL.KeyHashObj (SL.KeyHash h) ->
        W.RewardAccount (hashToBytes h)

fromPoolKeyHash :: SL.KeyHash rol sc -> W.PoolId
fromPoolKeyHash (SL.KeyHash h) =
    W.PoolId (hashToBytes h)

fromOwnerKeyHash :: SL.KeyHash 'SL.Staking crypto -> W.PoolOwner
fromOwnerKeyHash (SL.KeyHash h) =
    W.PoolOwner (hashToBytes h)

fromUnitInterval :: HasCallStack => SL.UnitInterval -> Percentage
fromUnitInterval x =
    either bomb id . mkPercentage . toRational . SL.unboundRational $ x
  where
    bomb = internalError $
        "fromUnitInterval: encountered invalid parameter value: "+||x||+""

toBccTxId :: W.Hash "Tx" -> Bcc.TxId
toBccTxId (W.Hash h) = Bcc.TxId $ UnsafeHash $ toShort h

toBccTxIn :: W.TxIn -> Bcc.TxIn
toBccTxIn (W.TxIn tid ix) =
    Bcc.TxIn (toBccTxId tid) (Bcc.TxIx (fromIntegral ix))

toBccStakeCredential :: W.RewardAccount -> Bcc.StakeCredential
toBccStakeCredential = Bcc.StakeCredentialByKey
    . Bcc.StakeKeyHash
    . SL.KeyHash
    . UnsafeHash
    . SBS.toShort
    . W.unRewardAccount

toBccEntropic :: W.Coin -> Bcc.Entropic
toBccEntropic (W.Coin c) = Bcc.Entropic $ safeCast c
  where
    safeCast :: Word64 -> Integer
    safeCast = fromIntegral

toBccTxOut :: SophieBasedEra era -> W.TxOut -> Bcc.TxOut era
toBccTxOut era = case era of
    SophieBasedEraSophie -> toSophieTxOut
    SophieBasedEraEvie -> toEvieTxOut
    SophieBasedEraJen    -> toJenTxOut
    SophieBasedEraAurum  -> toAurumTxOut
  where
    toSophieTxOut :: HasCallStack => W.TxOut -> Bcc.TxOut SophieEra
    toSophieTxOut (W.TxOut (W.Address addr) tokens) =
        Bcc.TxOut
            addrInEra
            (bccOnly $ toBccEntropic $ TokenBundle.getCoin tokens)
            Bcc.TxOutDatumHashNone
      where
        bccOnly = Bcc.TxOutBccOnly Bcc.BccOnlyInSophieEra
        addrInEra = tina "toBccTxOut: malformed address"
            [ Bcc.AddressInEra
                (Bcc.SophieAddressInEra Bcc.SophieBasedEraSophie)
                <$> deserialiseFromRawBytes AsSophieAddress addr

            , Bcc.AddressInEra Bcc.ColeAddressInAnyEra
                <$> deserialiseFromRawBytes AsColeAddress addr
            ]

    toEvieTxOut :: HasCallStack => W.TxOut -> Bcc.TxOut EvieEra
    toEvieTxOut (W.TxOut (W.Address addr) tokens) =
        Bcc.TxOut
            addrInEra
            (bccOnly $ toBccEntropic $ TokenBundle.getCoin tokens)
            Bcc.TxOutDatumHashNone
      where
        bccOnly = Bcc.TxOutBccOnly Bcc.BccOnlyInEvieEra
        addrInEra = tina "toBccTxOut: malformed address"
            [ Bcc.AddressInEra
                (Bcc.SophieAddressInEra Bcc.SophieBasedEraEvie)
                <$> deserialiseFromRawBytes AsSophieAddress addr

            , Bcc.AddressInEra Bcc.ColeAddressInAnyEra
                <$> deserialiseFromRawBytes AsColeAddress addr
            ]

    toJenTxOut :: HasCallStack => W.TxOut -> Bcc.TxOut JenEra
    toJenTxOut (W.TxOut (W.Address addr) tokens) =
        Bcc.TxOut
            addrInEra
            (Bcc.TxOutValue Bcc.MultiAssetInJenEra $ toBccValue tokens)
            Bcc.TxOutDatumHashNone
      where
        addrInEra = tina "toBccTxOut: malformed address"
            [ Bcc.AddressInEra (Bcc.SophieAddressInEra Bcc.SophieBasedEraJen)
                <$> deserialiseFromRawBytes AsSophieAddress addr

            , Bcc.AddressInEra Bcc.ColeAddressInAnyEra
                <$> deserialiseFromRawBytes AsColeAddress addr
            ]

    toAurumTxOut :: HasCallStack => W.TxOut -> Bcc.TxOut AurumEra
    toAurumTxOut (W.TxOut (W.Address addr) tokens) =
        Bcc.TxOut
            addrInEra
            (Bcc.TxOutValue Bcc.MultiAssetInAurumEra $ toBccValue tokens)
            datumHash
      where
        datumHash = Bcc.TxOutDatumHashNone
        addrInEra = tina "toBccTxOut: malformed address"
            [ Bcc.AddressInEra (Bcc.SophieAddressInEra Bcc.SophieBasedEraAurum)
                <$> deserialiseFromRawBytes AsSophieAddress addr

            , Bcc.AddressInEra Bcc.ColeAddressInAnyEra
                <$> deserialiseFromRawBytes AsColeAddress addr
            ]

toBccValue :: HasCallStack => TokenBundle.TokenBundle -> Bcc.Value
toBccValue tb = Bcc.valueFromList $
    (Bcc.BccAssetId, coinToQuantity coin) :
    map (bimap toBccAssetId toQuantity) bundle
  where
    (coin, bundle) = TokenBundle.toFlatList tb
    toBccAssetId (TokenBundle.AssetId pid name) =
        Bcc.AssetId (toBccPolicyId pid) (toBccAssetName name)

    toBccPolicyId (W.UnsafeTokenPolicyId (W.Hash pid)) = just "PolicyId"
        [Bcc.deserialiseFromRawBytes Bcc.AsPolicyId pid]
    toBccAssetName (W.UnsafeTokenName name) = just "TokenName"
        [Bcc.deserialiseFromRawBytes Bcc.AsAssetName name]

    just :: Builder -> [Maybe a] -> a
    just t = tina ("toBccValue: unable to deserialise "+|t)

    coinToQuantity = fromIntegral . W.unCoin
    toQuantity = fromIntegral . W.unTokenQuantity

-- | Convert from reward account address (which is a hash of a public key)
-- to a sophie ledger stake credential.
toStakeCredential
    :: (Crypto.HashAlgorithm (SL.ADDRHASH crypto))
    => W.RewardAccount
    -> SL.StakeCredential crypto
toStakeCredential = SL.KeyHashObj
    . SL.KeyHash . unsafeHashFromBytes . W.unRewardAccount

unsafeHashFromBytes :: Crypto.HashAlgorithm h => ByteString -> Hash h a
unsafeHashFromBytes =
    fromMaybe (error "unsafeHashFromBytes: wrong length")
    . Crypto.hashFromBytes

toStakeKeyDeregCert :: XPub -> Bcc.Certificate
toStakeKeyDeregCert = Bcc.makeStakeAddressDeregistrationCertificate
    . Bcc.StakeCredentialByKey
    . Bcc.StakeKeyHash
    . SL.KeyHash
    . UnsafeHash
    . toShort
    . blake2b224
    . xpubPublicKey

toStakeKeyRegCert :: XPub -> Bcc.Certificate
toStakeKeyRegCert = Bcc.makeStakeAddressRegistrationCertificate
    . Bcc.StakeCredentialByKey
    . Bcc.StakeKeyHash
    . SL.KeyHash
    . UnsafeHash
    . toShort
    . blake2b224
    . xpubPublicKey

toStakePoolDlgCert :: XPub -> W.PoolId -> Bcc.Certificate
toStakePoolDlgCert xpub (W.PoolId pid) =
    Bcc.makeStakeAddressDelegationCertificate
        (Bcc.StakeCredentialByKey $ Bcc.StakeKeyHash cred)
        (Bcc.StakePoolKeyHash pool)
  where
    cred = SL.KeyHash $ UnsafeHash $ toShort $ blake2b224 $ xpubPublicKey xpub
    pool = SL.KeyHash $ UnsafeHash $ toShort pid


-- | Extract a stake reference / `RewardAccount` from an address, if it exists.
--
-- Note that this returns `Nothing` for pointer addresses, not just enterprise
-- addresses.
rewardAccountFromAddress :: W.Address -> Maybe W.RewardAccount
rewardAccountFromAddress (W.Address bytes) = refToAccount . ref =<< parseAddr bytes
  where
    parseAddr :: ByteString -> Maybe (Bcc.Address Bcc.SophieAddr)
    parseAddr = Bcc.deserialiseFromRawBytes AsSophieAddress

    ref :: Bcc.Address Bcc.SophieAddr -> SL.StakeReference StandardCrypto
    ref (Bcc.SophieAddress _n _paymentKey stakeRef) = stakeRef

    refToAccount :: SL.StakeReference StandardCrypto -> Maybe W.RewardAccount
    refToAccount (SL.StakeRefBase cred) = Just $ fromStakeCredential cred
    refToAccount (SL.StakeRefPtr _) = Nothing
    refToAccount SL.StakeRefNull = Nothing

-- | Converts 'SealedTx' to something that can be submitted with the
-- 'Bcc.Api' local tx submission client.
unsealSophieTx :: W.SealedTx -> TxInMode BccMode
unsealSophieTx wtx = case W.bccTx wtx of
    Bcc.InAnyBccEra ColeEra tx ->
        TxInMode tx ColeEraInBccMode
    Bcc.InAnyBccEra SophieEra tx ->
        TxInMode tx SophieEraInBccMode
    Bcc.InAnyBccEra EvieEra tx ->
        TxInMode tx EvieEraInBccMode
    Bcc.InAnyBccEra JenEra tx ->
        TxInMode tx JenEraInBccMode
    Bcc.InAnyBccEra AurumEra tx ->
        TxInMode tx AurumEraInBccMode

-- | Converts a 'SophieBasedEra' to the broader 'BccEra'.
sophieBasedToBccEra :: SophieBasedEra era -> BccEra era
sophieBasedToBccEra Bcc.SophieBasedEraSophie = SophieEra
sophieBasedToBccEra Bcc.SophieBasedEraEvie = EvieEra
sophieBasedToBccEra Bcc.SophieBasedEraJen    = JenEra
sophieBasedToBccEra Bcc.SophieBasedEraAurum  = AurumEra

-- | An existential type like 'AnyBccEra', but for 'SophieBasedEra'.
data AnySophieBasedEra where
     AnySophieBasedEra :: IsSophieBasedEra era -- Provide class constraint
                        => SophieBasedEra era   -- and explicit value.
                        -> AnySophieBasedEra    -- and that's it.

instance Show AnySophieBasedEra where
    show (AnySophieBasedEra era) = "AnySophieBasedEra " ++ show era

anySophieBasedEra :: InAnySophieBasedEra (Const ()) -> AnySophieBasedEra
anySophieBasedEra (InAnySophieBasedEra era _) = AnySophieBasedEra era

sophieToBccEra :: AnySophieBasedEra -> AnyBccEra
sophieToBccEra (AnySophieBasedEra era) =
    AnyBccEra (sophieBasedToBccEra era)

getSophieBasedEra :: AnyBccEra -> Maybe AnySophieBasedEra
getSophieBasedEra (AnyBccEra e) = case bccEraStyle e of
    LegacyColeEra -> Nothing
    SophieBasedEra era -> Just
        (anySophieBasedEra (InAnySophieBasedEra era (Const ())))

instance (forall era. IsBccEra era => Show (thing era)) =>
    Show (InAnyBccEra thing) where
    show (InAnyBccEra era thing) =
        "InAnyBccEra " ++ show era ++ " (" ++ show thing ++ ")"

instance (forall era. IsBccEra era => Eq (thing era)) =>
    Eq (InAnyBccEra thing) where
    InAnyBccEra e1 a == InAnyBccEra e2 b = case testEquality e1 e2 of
        Just Refl -> a == b
        Nothing -> False

{-------------------------------------------------------------------------------
                   Assessing sizes of token bundles
-------------------------------------------------------------------------------}

-- | Assesses a token bundle size in relation to the maximum size that can be
--   included in a transaction output.
--
-- See 'W.TokenBundleSizeAssessor' for the expected properties of this function.
--
tokenBundleSizeAssessor :: W.TokenBundleMaxSize -> W.TokenBundleSizeAssessor
tokenBundleSizeAssessor maxSize = W.TokenBundleSizeAssessor {..}
  where
    assessTokenBundleSize tb
        | serializedLengthBytes <= maxSize' =
            W.TokenBundleSizeWithinLimit
        | otherwise =
            W.OutputTokenBundleSizeExceedsLimit
      where
        serializedLengthBytes :: W.TxSize
        serializedLengthBytes = computeTokenBundleSerializedLengthBytes tb

        maxSize' :: W.TxSize
        maxSize' = W.unTokenBundleMaxSize maxSize

computeTokenBundleSerializedLengthBytes :: TokenBundle.TokenBundle -> W.TxSize
computeTokenBundleSerializedLengthBytes = W.TxSize . safeCast
    . BS.length . Binary.serialize' . Bcc.toJenValue . toBccValue
  where
    safeCast :: Int -> Natural
    safeCast = fromIntegral

{-------------------------------------------------------------------------------
                      Address Encoding / Decoding
-------------------------------------------------------------------------------}

instance EncodeStakeAddress 'Mainnet where
    encodeStakeAddress = _encodeStakeAddress SL.Mainnet
instance EncodeStakeAddress ('Testnet pm) where
    encodeStakeAddress = _encodeStakeAddress SL.Testnet

instance DecodeStakeAddress 'Mainnet where
    decodeStakeAddress = _decodeStakeAddress SL.Mainnet
instance DecodeStakeAddress ('Testnet pm) where
    decodeStakeAddress = _decodeStakeAddress SL.Testnet

stakeAddressPrefix :: Word8
stakeAddressPrefix = 0xE0

networkIdMask :: Word8
networkIdMask = 0x0F

toNetworkId :: SL.Network -> Word8
toNetworkId = \case
    SL.Testnet -> 0
    SL.Mainnet -> 1

_encodeStakeAddress
    :: SL.Network
    -> W.RewardAccount
    -> Text
_encodeStakeAddress network (W.RewardAccount acct) =
    Bech32.encodeLenient hrp (dataPartFromBytes bytes)
  where
    hrp = case network of
        SL.Testnet -> [Bech32.humanReadablePart|stake_test|]
        SL.Mainnet -> [Bech32.humanReadablePart|stake|]
    bytes = BL.toStrict $ runPut $ do
        putWord8 $ (networkIdMask .&. toNetworkId network) .|. stakeAddressPrefix
        putByteString acct

_decodeStakeAddress
    :: SL.Network
    -> Text
    -> Either TextDecodingError W.RewardAccount
_decodeStakeAddress serverNetwork txt = do
    (_, dp) <- left (const errBech32) $ Bech32.decodeLenient txt
    bytes <- maybe (Left errBech32) Right $ dataPartToBytes dp
    rewardAcnt <- runGetOrFail' (SL.getRewardAcnt @StandardCrypto) bytes

    guardNetwork (SL.getRwdNetwork rewardAcnt) serverNetwork

    pure $ fromStakeCredential $ SL.getRwdCred rewardAcnt
  where
    runGetOrFail' decoder bytes =
        case runGetOrFail decoder (BL.fromStrict bytes) of
            Left e ->
                Left (TextDecodingError (show e))

            Right (remaining,_,_) | not (BL.null remaining) ->
                Left errDecode

            Right (_,_,a) ->
                Right a

    errDecode = TextDecodingError
        "Unable to decode stake-address: not a well-formed address."

    errBech32 = TextDecodingError
        "Unable to decode stake-address: must be a valid bech32 string."

instance EncodeAddress 'Mainnet where
    encodeAddress = _encodeAddress [Bech32.humanReadablePart|addr|]

instance EncodeAddress ('Testnet pm) where
    -- https://github.com/the-blockchain-company/CIPs/tree/master/CIP5
    encodeAddress = _encodeAddress [Bech32.humanReadablePart|addr_test|]

_encodeAddress :: Bech32.HumanReadablePart -> W.Address -> Text
_encodeAddress hrp (W.Address bytes) =
    if isJust (CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes)
        then base58
        else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)

instance DecodeAddress 'Mainnet where
    decodeAddress = _decodeAddress SL.Mainnet

instance DecodeAddress ('Testnet pm) where
    decodeAddress = _decodeAddress SL.Testnet

decodeBytes :: Text -> Either TextDecodingError ByteString
decodeBytes t =
    case tryBase16 t <|> tryBech32 t <|> tryBase58 t of
        Just bytes ->
            Right bytes
        _ ->
            Left $ TextDecodingError
                "Unrecognized address encoding: must be either bech32, base58 or base16"

-- | Attempt decoding an 'Address' using a Bech32 encoding.
tryBech32 :: Text -> Maybe ByteString
tryBech32 text = do
    (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient text)
    dataPartToBytes dp

-- | Attempt decoding a legacy 'Address' using a Base58 encoding.
tryBase58 :: Text -> Maybe ByteString
tryBase58 text =
    decodeBase58 bitcoinAlphabet (T.encodeUtf8 text)

-- | Attempt decoding an 'Address' using Base16 encoding
tryBase16 :: Text -> Maybe ByteString
tryBase16 text =
    either (const Nothing) Just $ convertFromBase Base16 (T.encodeUtf8 text)

errMalformedAddress :: TextDecodingError
errMalformedAddress = TextDecodingError
    "Unable to decode address: not a well-formed Sophie nor Cole address."

-- Note that for 'Cole', we always assume no discrimination. In
-- practice, there is one discrimination for 'Sophie' addresses, and one for
-- 'Cole' addresses. Yet, on Mainnet, 'Cole' addresses have no explicit
-- discrimination.
_decodeAddress
    :: SL.Network
    -> Text
    -> Either TextDecodingError W.Address
_decodeAddress serverNetwork =
    decodeBytes >=> decodeSophieAddress @StandardCrypto
  where
    decodeSophieAddress :: forall c. (SL.Crypto c) => ByteString -> Either TextDecodingError W.Address
    decodeSophieAddress bytes = do
        case SL.deserialiseAddr @c bytes of
            Just (SL.Addr addrNetwork _ _) -> do
                guardNetwork addrNetwork serverNetwork
                pure (W.Address bytes)

            Just (SL.AddrBootstrap (SL.BootstrapAddress addr)) -> do
                guardNetwork (toNetwork (Cole.addrNetworkMagic addr)) serverNetwork
                pure (W.Address bytes)

            Nothing -> Left errMalformedAddress

      where
        toNetwork :: Cole.NetworkMagic -> SL.Network
        toNetwork = \case
            Cole.NetworkMainOrStage -> SL.Mainnet
            Cole.NetworkTestnet{}   -> SL.Testnet

-- FIXME: 'bcc-addresses' currently gives us an opaque 'Value'. It'd be
-- nicer to model this as a proper Haskell type and to serialize in due times.
inspectAddress
    :: Text
    -> Either TextDecodingError Aeson.Value
inspectAddress =
    decodeBytes >=> inspect
  where
    inspect :: ByteString -> Either TextDecodingError Aeson.Value
    inspect = maybe (Left errMalformedAddress) Right
        . CA.inspectAddress mRootPub
        . unsafeMkAddress
    -- TODO: It's possible to inspect a cole address, given a root XPub.
    -- However, this is not yet exposed by the API.
    mRootPub = Nothing

toHDPayloadAddress :: W.Address -> Maybe Cole.HDAddressPayload
toHDPayloadAddress (W.Address addr) = do
    payload <- CBOR.deserialiseCbor CBOR.decodeAddressPayload addr
    attributes <- CBOR.deserialiseCbor decodeAllAttributes' payload
    case filter (\(tag,_) -> tag == 1) attributes of
        [(1, bytes)] ->
            Cole.HDAddressPayload <$> CBOR.decodeNestedBytes CBOR.decodeBytes bytes
        _ ->
            Nothing
  where
    decodeAllAttributes' = do
        _ <- CBOR.decodeListLenCanonicalOf 3
        _ <- CBOR.decodeBytes
        CBOR.decodeAllAttributes

guardNetwork :: SL.Network -> SL.Network -> Either TextDecodingError ()
guardNetwork addrNetwork serverNetwork =
    when (addrNetwork /= serverNetwork) $
        Left $ TextDecodingError $
            "Invalid network discrimination on address. Expecting "
            <> show serverNetwork
            <> " but got "
            <> show addrNetwork
            <> "."

-- | Class to extract a @NetworkId@ from @NetworkDiscriminant@.
class HasNetworkId (n :: NetworkDiscriminant) where
    networkIdVal :: Proxy n -> NetworkId

instance HasNetworkId 'Mainnet where
    networkIdVal _ = Bcc.Mainnet

instance KnownNat protocolMagic => HasNetworkId ('Testnet protocolMagic) where
    networkIdVal _ = Bcc.Testnet networkMagic
      where
        networkMagic = Bcc.NetworkMagic
            . fromIntegral
            $ natVal (Proxy @protocolMagic)

instance HasNetworkId ('Staging protocolMagic) where
    networkIdVal _ = Bcc.Mainnet

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- Compact representation of connection id for log messages.
instance Buildable addr => Buildable (ConnectionId addr) where
   build (ConnectionId a b) = "conn:" <> build a <> ":" <> build b

instance Buildable LocalAddress where
    build (LocalAddress p) = build p

{-------------------------------------------------------------------------------
                                 Utilities
-------------------------------------------------------------------------------}

-- Inverts a value in the unit interval [0, 1].
--
-- Examples:
--
-- >>> invertUnitInterval interval0 == interval1
-- >>> invertUnitInterval interval1 == interval0
--
-- Satisfies the following properties:
--
-- >>> invertUnitInterval . invertUnitInterval == id
-- >>> intervalValue (invertUnitInterval i) + intervalValue i == 1
--
invertUnitInterval :: HasCallStack => SL.UnitInterval -> SL.UnitInterval
invertUnitInterval = unsafeBoundRational . (1 - ) . SL.unboundRational
  where
    unsafeBoundRational :: Rational -> SL.UnitInterval
    unsafeBoundRational = tina "invertUnitInterval: the impossible happened"
        . pure . SL.boundRational

interval1 :: SL.UnitInterval
interval1 = maxBound

interval0 :: SL.UnitInterval
interval0 = minBound
