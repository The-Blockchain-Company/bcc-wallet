{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Sophie.Network
    ( spec
    ) where

import Prelude

import Bcc.Wallet.Api.Types
    ( ApiEpochInfo, ApiEra (..), ApiNetworkParameters (..) )
import Bcc.Wallet.Primitive.Types
    ( ExecutionUnitPrices (..) )
import Data.List
    ( (\\) )
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( Expectation, SpecWith, describe, shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , RequestException
    , counterexample
    , epochLengthValue
    , expectField
    , expectResponseCode
    , maximumCollateralInputCountByEra
    , minUTxOValue
    , request
    , securityParameterValue
    , slotLengthValue
    , verify
    )

import qualified Bcc.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = describe "SOPHIE_NETWORK" $ do
    it "NETWORK_PARAMS - Able to fetch network parameters" $ \ctx -> do
        r <- request @ApiNetworkParameters ctx Link.getNetworkParams Default Empty
        expectResponseCode @IO HTTP.status200 r
        let Right d = Quantity <$> mkPercentage (3 % 4)
        -- for Sophie desiredPoolNumber is node's nOpt protocol parameter
        -- in integration test setup it is 3
        let nOpt = 3
        let
            expectEraField
                :: (Maybe ApiEpochInfo -> Expectation)
                -> ApiEra
                -> (HTTP.Status, Either RequestException ApiNetworkParameters)
                -> IO ()
            expectEraField toBe era = counterexample ("For era: " <> show era)
                . case era of
                    ApiCole -> expectField (#eras . #cole) toBe
                    ApiSophie -> expectField (#eras . #sophie) toBe
                    ApiEvie -> expectField (#eras . #evie) toBe
                    ApiJen -> expectField (#eras . #jen) toBe
                    ApiAurum -> expectField (#eras . #aurum) toBe

        let knownEras = [minBound .. _mainEra ctx]
        let unknownEras = [minBound .. maxBound] \\ knownEras
        -- exec prices values from aurum-genesis.yml
        let execUnitPrices = Just (ExecutionUnitPrices
                                      {priceExecutionSteps = 577 % 10000,
                                       priceExecutionMemory = 721 % 10000000})
        let checkExecutionUnitPricesPresence
                :: ApiEra
                -> ((HTTP.Status, Either RequestException ApiNetworkParameters) -> IO ())
            checkExecutionUnitPricesPresence = \case
                ApiAurum -> expectField #executionUnitPrices (`shouldBe` execUnitPrices)
                _ -> expectField #executionUnitPrices (`shouldBe` Nothing)

        verify r $
            [ expectField #decentralizationLevel (`shouldBe` d)
            , expectField #desiredPoolNumber (`shouldBe` nOpt)
            , expectField #minimumUtxoValue (`shouldBe` Quantity (minUTxOValue (_mainEra ctx)))
            , expectField #slotLength (`shouldBe` Quantity slotLengthValue)
            , expectField #epochLength (`shouldBe` Quantity epochLengthValue)
            , expectField #securityParameter (`shouldBe` Quantity securityParameterValue)
            , expectField #activeSlotCoefficient (`shouldBe` Quantity 50.0)
            , expectField #maximumCollateralInputCount
                  (`shouldBe` maximumCollateralInputCountByEra (_mainEra ctx))
            , expectField #maximumTokenBundleSize (`shouldBe` Quantity 5000)
            , checkExecutionUnitPricesPresence (_mainEra ctx)
            ]
            ++ map (expectEraField (`shouldNotBe` Nothing)) knownEras
            ++ map (expectEraField (`shouldBe` Nothing)) unknownEras
