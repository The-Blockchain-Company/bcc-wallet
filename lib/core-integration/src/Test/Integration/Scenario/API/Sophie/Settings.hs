{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Sophie.Settings
    ( spec
    ) where

import Prelude

import Bcc.Wallet.Api.Types
    ( ApiStakePool
    , ApiT (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    )
import Bcc.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Bcc.Wallet.Primitive.AddressDerivation.Cole
    ( ColeKey )
import Bcc.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Bcc.Wallet.Primitive.AddressDerivation.Sophie
    ( SophieKey )
import Bcc.Wallet.Primitive.Types
    ( PoolMetadataSource (..), Settings )
import Bcc.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Data.Either
    ( fromRight )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Maybe
    ( isJust, isNothing )
import Data.Text.Class
    ( fromText )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , eventuallyUsingDelay
    , expectField
    , expectResponseCode
    , request
    , unsafeRequest
    , updateMetadataSource
    , verify
    , verifyMetadataSource
    )

import qualified Bcc.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n SophieKey
    , PaymentAddress n IcarusKey
    , PaymentAddress n ColeKey
    ) => SpecWith Context
spec = describe "SOPHIE_SETTINGS" $ do
    it "SETTINGS_01 - Can put and read settings" $ \ctx -> do
        let uri = "http://smash.it"
        updateMetadataSource ctx uri
        eventually "The settings are applied" $ do
            r2 <- request @(ApiT Settings) ctx Link.getSettings Default Empty
            verify r2
                [ expectResponseCode HTTP.status200
                , expectField (#getApiT . #poolMetadataSource)
                    (`shouldBe` (fromRight (error "no") $ fromText
                        @PoolMetadataSource uri))
                ]

    it "SETTINGS_02 - Changing pool_metadata_source re-syncs metadata" $ \ctx -> do
        let toNone = "none"
            toDirect = "direct"
            getMetadata = fmap (view #metadata) . snd <$> unsafeRequest
                @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
            delay = 500 * 1000
            timeout = 120

        updateMetadataSource ctx toNone
        verifyMetadataSource ctx FetchNone
        eventuallyUsingDelay delay timeout "1. There is no metadata" $
            getMetadata >>= (`shouldSatisfy` all isNothing)

        updateMetadataSource ctx toDirect
        verifyMetadataSource ctx FetchDirect
        eventuallyUsingDelay delay timeout "2. There is metadata" $
            getMetadata >>= (`shouldSatisfy` all isJust)

        updateMetadataSource ctx toNone
        verifyMetadataSource ctx FetchNone
        eventuallyUsingDelay delay timeout "3. There is no metadata" $
            getMetadata >>= (`shouldSatisfy` all isNothing)

arbitraryStake :: Maybe Coin
arbitraryStake = Just $ bcc 10_000
  where bcc = Coin . (1000*1000*)
