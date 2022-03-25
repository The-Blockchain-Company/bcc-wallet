{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2021-2022 TBCO
-- License: Apache-2.0
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Bcc.Wallet.Sophie.Api.Server
    ( server
    ) where

import Prelude

import Bcc.Address
    ( unAddress )
import Bcc.Address.Script
    ( prettyErrValidateScript, validateScript )
import Bcc.Pool.Metadata
    ( defaultManagerSettings, healthCheck, newManager, toHealthCheckSMASH )
import Bcc.Wallet
    ( ErrCreateRandomAddress (..)
    , ErrNotASequentialWallet (..)
    , genesisData
    , networkLayer
    , normalizeDelegationAddress
    , normalizeSharedAddress
    , transactionLayer
    )
import Bcc.Wallet.Api
    ( Addresses
    , Api
    , ApiLayer (..)
    , Assets
    , ColeAddresses
    , ColeAssets
    , ColeCoinSelections
    , ColeMigrations
    , ColeTransactions
    , ColeWallets
    , CoinSelections
    , Network
    , Proxy_
    , SMASH
    , Settings
    , SharedAddresses
    , SharedWalletKeys
    , SharedWallets
    , SophieMigrations
    , SophieTransactions
    , StakePools
    , WalletKeys
    , Wallets
    )
import Bcc.Wallet.Api.Server
    ( apiError
    , balanceTransaction
    , constructTransaction
    , createMigrationPlan
    , delegationFee
    , deleteTransaction
    , deleteWallet
    , derivePublicKey
    , getAccountPublicKey
    , getAsset
    , getAssetDefault
    , getCurrentEpoch
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getTransaction
    , getUTxOsStatistics
    , getWallet
    , getWalletUtxoSnapshot
    , idleWorker
    , joinStakePool
    , liftHandler
    , listAddresses
    , listAssets
    , listStakeKeys
    , listTransactions
    , listWallets
    , migrateWallet
    , mintBurnAssets
    , mkLegacyWallet
    , mkSharedWallet
    , mkSophieWallet
    , patchSharedWallet
    , postAccountPublicKey
    , postAccountWallet
    , postExternalTransaction
    , postIcarusWallet
    , postLedgerWallet
    , postRandomAddress
    , postRandomWallet
    , postRandomWalletFromXPrv
    , postSharedWallet
    , postTransactionFeeOld
    , postTransactionOld
    , postTrezorWallet
    , postWallet
    , putColeWalletPassphrase
    , putRandomAddress
    , putRandomAddresses
    , putWallet
    , putWalletPassphrase
    , quitStakePool
    , rndStateChange
    , selectCoins
    , selectCoinsForJoin
    , selectCoinsForQuit
    , signMetadata
    , signTransaction
    , withLegacyLayer
    , withLegacyLayer'
    )
import Bcc.Wallet.Api.Types
    ( AnyAddress (..)
    , AnyAddressType (..)
    , ApiAccountKey (..)
    , ApiAccountKeyShared (..)
    , ApiAddressData (..)
    , ApiAddressDataPayload (..)
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
    , ApiCredential (..)
    , ApiDelegationAction (..)
    , ApiErrorCode (..)
    , ApiHealthCheck (..)
    , ApiMaintenanceAction (..)
    , ApiMaintenanceActionPostData (..)
    , ApiPostAccountKeyData (..)
    , ApiPostAccountKeyDataWithPurpose (..)
    , ApiSelectCoinsAction (..)
    , ApiSelectCoinsData (..)
    , ApiStakePool
    , ApiT (..)
    , ApiVerificationKeyShared (..)
    , ApiVerificationKeySophie (..)
    , ApiWithdrawalPostData (..)
    , HealthCheckSMASH (..)
    , MaintenanceAction (..)
    , SettingsPutData (..)
    , SomeColeWalletPostData (..)
    )
import Bcc.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..), PaymentAddress (..) )
import Bcc.Wallet.Primitive.AddressDerivation.Cole
    ( ColeKey )
import Bcc.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Bcc.Wallet.Primitive.AddressDerivation.Shared
    ( SharedKey (..) )
import Bcc.Wallet.Primitive.AddressDerivation.Sophie
    ( SophieKey (..) )
import Bcc.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Bcc.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Bcc.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..), SharedState )
import Bcc.Wallet.Primitive.Types
    ( PoolMetadataSource (..), SmashServer (..), poolMetadataSource )
import Bcc.Wallet.Sophie.Compatibility
    ( HasNetworkId (..), NetworkId, inspectAddress, rewardAccountFromAddress )
import Bcc.Wallet.Sophie.Pools
    ( StakePoolLayer (..) )
import Control.Applicative
    ( liftA2 )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( except, throwE, withExceptT )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( sortOn )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Network.Ntp
    ( NtpClient )
import Servant
    ( (:<|>) (..), Handler (..), NoContent (..), Server, err400 )
import Servant.Server
    ( ServerError (..) )
import Type.Reflection
    ( Typeable )

import qualified Bcc.Address.Derivation as CA
import qualified Bcc.Address.Script as CA
import qualified Bcc.Address.Style.Sophie as CA
import qualified Bcc.Api as Bcc
import qualified Bcc.Wallet.Primitive.AddressDerivation.Shared as Shared
import qualified Bcc.Wallet.Primitive.AddressDerivation.Sophie as Sophie
import qualified Data.ByteString as BS
import qualified Data.Text as T

server
    :: forall n.
        ( PaymentAddress n IcarusKey
        , PaymentAddress n ColeKey
        , DelegationAddress n SophieKey
        , Typeable n
        , HasNetworkId n
        )
    => ApiLayer (RndState n) ColeKey
    -> ApiLayer (SeqState n IcarusKey) IcarusKey
    -> ApiLayer (SeqState n SophieKey) SophieKey
    -> ApiLayer (SharedState n SharedKey) SharedKey
    -> StakePoolLayer
    -> NtpClient
    -> Server (Api n ApiStakePool)
server cole icarus sophie multisig spl ntp =
         wallets
    :<|> walletKeys
    :<|> assets
    :<|> addresses
    :<|> coinSelections
    :<|> sophieTransactions
    :<|> sophieMigrations
    :<|> stakePools
    :<|> coleWallets
    :<|> coleAssets
    :<|> coleAddresses
    :<|> coleCoinSelections
    :<|> coleTransactions
    :<|> coleMigrations
    :<|> network'
    :<|> proxy
    :<|> settingS
    :<|> smash
    :<|> sharedWallets multisig
    :<|> sharedWalletKeys multisig
    :<|> sharedAddresses multisig
  where
    wallets :: Server Wallets
    wallets = deleteWallet sophie
        :<|> (fmap fst . getWallet sophie mkSophieWallet)
        :<|> (fmap fst <$> listWallets sophie mkSophieWallet)
        :<|> postWallet sophie Sophie.generateKeyFromSeed SophieKey
        :<|> putWallet sophie mkSophieWallet
        :<|> putWalletPassphrase sophie
        :<|> getWalletUtxoSnapshot sophie
        :<|> getUTxOsStatistics sophie

    walletKeys :: Server WalletKeys
    walletKeys = derivePublicKey sophie ApiVerificationKeySophie
        :<|> signMetadata sophie
        :<|> postAccountPublicKey sophie ApiAccountKey
        :<|> getAccountPublicKey sophie ApiAccountKey

    assets :: Server (Assets n)
    assets =
        mintBurnAssets sophie
        :<|> listAssets sophie
        :<|> getAsset sophie
        :<|> getAssetDefault sophie

    addresses :: Server (Addresses n)
    addresses = listAddresses sophie (normalizeDelegationAddress @_ @SophieKey @n)
        :<|> (handler ApiAddressInspect . inspectAddress . unApiAddressInspectData)
        :<|> (handler id . postAnyAddress (networkIdVal (Proxy @n)))
      where
        toServerError :: TextDecodingError -> ServerError
        toServerError = apiError err400 BadRequest . T.pack . getTextDecodingError

        handler :: (a -> result) -> Either TextDecodingError a -> Handler result
        handler transform =
            Handler . withExceptT toServerError . except . fmap transform

    -- Hlint doesn't seem to care about inlining properties:
    --   https://github.com/quchen/articles/blob/master/fbut.md#f-x---is-not-f--x---
    {- HLINT ignore "Redundant lambda" -}
    coinSelections :: Server (CoinSelections n)
    coinSelections = (\wid ascd -> case ascd of
        (ApiSelectForPayment ascp) ->
            selectCoins sophie (delegationAddress @n) wid ascp
        (ApiSelectForDelegation (ApiSelectCoinsAction action)) ->
            case action of
                (Join pid) ->
                    selectCoinsForJoin
                        sophie
                        (knownPools spl)
                        (getPoolLifeCycleStatus spl)
                        (getApiT pid)
                        (getApiT wid)
                Quit ->
                    selectCoinsForQuit sophie wid
        )

    sophieTransactions :: Server (SophieTransactions n)
    sophieTransactions =
             constructTransaction sophie (delegationAddress @n)
        :<|> signTransaction sophie
        :<|> listTransactions sophie
        :<|> getTransaction sophie
        :<|> deleteTransaction sophie
        :<|> postTransactionOld sophie (delegationAddress @n)
        :<|> postTransactionFeeOld sophie
        :<|> balanceTransaction sophie

    sophieMigrations :: Server (SophieMigrations n)
    sophieMigrations =
             createMigrationPlan @_ @_ sophie (Just SelfWithdrawal)
        :<|> migrateWallet sophie (Just SelfWithdrawal)

    stakePools :: Server (StakePools n ApiStakePool)
    stakePools =
        listStakePools_
        :<|> joinStakePool sophie (knownPools spl) (getPoolLifeCycleStatus spl)
        :<|> quitStakePool sophie
        :<|> delegationFee sophie
        :<|> listStakeKeys rewardAccountFromAddress sophie
        :<|> postPoolMaintenance
        :<|> getPoolMaintenance
      where
        listStakePools_ = \case
            Just (ApiT stake) -> do
                currentEpoch <- getCurrentEpoch sophie
                liftIO $ listStakePools spl currentEpoch stake
            Nothing -> Handler $ throwE $ apiError err400 QueryParamMissing $
                mconcat
                [ "The stake intended to delegate must be provided as a query "
                , "parameter as it affects the rewards and ranking."
                ]

        postPoolMaintenance action' = do
            case action' of
                ApiMaintenanceActionPostData GcStakePools ->
                    liftIO $ forceMetadataGC spl
            pure NoContent

        getPoolMaintenance =
            liftIO (ApiMaintenanceAction . ApiT <$> getGCMetadataStatus spl)

    coleWallets :: Server ColeWallets
    coleWallets =
        (\case
            RandomWalletFromMnemonic x -> postRandomWallet cole x
            RandomWalletFromXPrv x -> postRandomWalletFromXPrv cole x
            SomeIcarusWallet x -> postIcarusWallet icarus x
            SomeTrezorWallet x -> postTrezorWallet icarus x
            SomeLedgerWallet x -> postLedgerWallet icarus x
            SomeAccount x ->
                postAccountWallet icarus mkLegacyWallet IcarusKey idleWorker x
        )
        :<|> (\wid -> withLegacyLayer wid
                (cole , deleteWallet cole wid)
                (icarus, deleteWallet icarus wid)
             )
        :<|> (\wid -> withLegacyLayer' wid
                ( cole
                , fst <$> getWallet cole mkLegacyWallet wid
                , const (fst <$> getWallet cole mkLegacyWallet wid)
                )
                ( icarus
                , fst <$> getWallet icarus mkLegacyWallet wid
                , const (fst <$> getWallet icarus mkLegacyWallet wid)
                )
             )
        :<|> liftA2 (\xs ys -> fmap fst $ sortOn snd $ xs ++ ys)
            (listWallets cole  mkLegacyWallet)
            (listWallets icarus mkLegacyWallet)
        :<|> (\wid name -> withLegacyLayer wid
                (cole , putWallet cole mkLegacyWallet wid name)
                (icarus, putWallet icarus mkLegacyWallet wid name)
             )
        :<|> (\wid -> withLegacyLayer wid
                (cole , getWalletUtxoSnapshot cole wid)
                (icarus, getWalletUtxoSnapshot icarus wid)
             )
        :<|> (\wid -> withLegacyLayer wid
                (cole , getUTxOsStatistics cole wid)
                (icarus, getUTxOsStatistics icarus wid)
             )
        :<|> (\wid pwd -> withLegacyLayer wid
                (cole , putColeWalletPassphrase cole wid pwd)
                (icarus, putColeWalletPassphrase icarus wid pwd)
             )

    coleAssets :: Server ColeAssets
    coleAssets =
            (\wid -> withLegacyLayer wid
                (cole, listAssets cole wid)
                (icarus, listAssets icarus wid)
            )
        :<|> (\wid t n -> withLegacyLayer wid
                (cole, getAsset cole wid t n)
                (icarus, getAsset icarus wid t n)
            )
        :<|> (\wid t -> withLegacyLayer wid
                (cole, getAssetDefault cole wid t)
                (icarus, getAssetDefault icarus wid t)
            )

    coleAddresses :: Server (ColeAddresses n)
    coleAddresses =
             (\wid s -> withLegacyLayer wid
                (cole, postRandomAddress cole wid s)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAColeWallet)
             )
        :<|> (\wid addr -> withLegacyLayer wid
                (cole, putRandomAddress cole wid addr)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAColeWallet)
             )
        :<|> (\wid s -> withLegacyLayer wid
                (cole, putRandomAddresses cole wid s)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAColeWallet)
             )
        :<|> (\wid s -> withLegacyLayer wid
                (cole , listAddresses cole (const pure) wid s)
                (icarus, listAddresses icarus (const pure) wid s)
             )

    coleCoinSelections :: Server (ColeCoinSelections n)
    coleCoinSelections wid (ApiSelectForPayment x) =
        withLegacyLayer wid (cole, handleRandom) (icarus, handleSequential)
      where
        handleRandom = liftHandler $ throwE ErrNotASequentialWallet
        handleSequential = selectCoins icarus genChangeSequential wid x
        genChangeSequential paymentK _ = paymentAddress @n paymentK
    coleCoinSelections _ _ = Handler
        $ throwE
        $ apiError err400 InvalidWalletType
        "Cole wallets don't have delegation capabilities."

    coleTransactions :: Server (ColeTransactions n)
    coleTransactions =
             (\wid tx -> withLegacyLayer wid
                 (cole , do
                    let pwd = error "fixme: unimplemented"
                    genChange <- rndStateChange cole wid pwd
                    constructTransaction cole genChange wid tx
                 )
                 (icarus, do
                    let genChange k _ = paymentAddress @n k
                    constructTransaction icarus genChange wid tx
                 )
             )
        :<|> (\wid tx ->
                 withLegacyLayer wid
                 (cole, signTransaction cole wid tx)
                 (icarus, signTransaction icarus wid tx)
             )
        :<|> (\wid r0 r1 s -> withLegacyLayer wid
                (cole , listTransactions cole wid Nothing r0 r1 s)
                (icarus, listTransactions icarus wid Nothing r0 r1 s)
             )
        :<|> (\wid txid -> withLegacyLayer wid
                (cole , getTransaction cole wid txid)
                (icarus, getTransaction icarus wid txid)
             )
        :<|> (\wid txid -> withLegacyLayer wid
                (cole , deleteTransaction cole wid txid)
                (icarus, deleteTransaction icarus wid txid)
             )
        :<|> (\wid tx -> withLegacyLayer wid
                 (cole , do
                    let pwd = coerce (getApiT $ tx ^. #passphrase)
                    genChange <- rndStateChange cole wid pwd
                    postTransactionOld cole genChange wid tx

                 )
                 (icarus, do
                    let genChange k _ = paymentAddress @n k
                    postTransactionOld icarus genChange wid tx
                 )
             )
       :<|> (\wid tx -> withLegacyLayer wid
                (cole , postTransactionFeeOld cole wid tx)
                (icarus, postTransactionFeeOld icarus wid tx)
            )

    coleMigrations :: Server (ColeMigrations n)
    coleMigrations =
             (\wid postData -> withLegacyLayer wid
                (cole , createMigrationPlan @_ @_ cole Nothing wid postData)
                (icarus, createMigrationPlan @_ @_ icarus Nothing wid postData)
             )
        :<|> (\wid m -> withLegacyLayer wid
                (cole , migrateWallet cole Nothing wid m)
                (icarus, migrateWallet icarus Nothing wid m)
             )

    network' :: Server Network
    network' =
        getNetworkInformation syncTolerance nl
        :<|> getNetworkParameters genesis nl tl
        :<|> getNetworkClock ntp
      where
        nl = icarus ^. networkLayer
        tl = icarus ^. transactionLayer @IcarusKey
        genesis@(_,_,syncTolerance) = icarus ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction icarus

    settingS :: Server Settings
    settingS = putSettings' :<|> getSettings'
      where
        putSettings' (SettingsPutData (ApiT settings'))
            = Handler $ do
                liftIO $ putSettings spl settings'
                pure NoContent
        getSettings'
            = Handler $ fmap ApiT $ liftIO $ getSettings spl

    smash :: Server SMASH
    smash = getCurrentSmashHealth
      where
        getHealth smashServer = liftIO $ do
            manager <- newManager defaultManagerSettings
            health' <- healthCheck mempty (unSmashServer smashServer) manager
            pure $ ApiHealthCheck $ toHealthCheckSMASH health'

        getCurrentSmashHealth (Just (ApiT smashServer)) = Handler $ getHealth smashServer
        getCurrentSmashHealth Nothing = Handler $ do
            settings' <- liftIO $ getSettings spl
            case poolMetadataSource settings' of
                FetchSMASH smashServer -> getHealth smashServer
                _ -> pure (ApiHealthCheck NoSmashConfigured)

    sharedWallets
        :: ApiLayer (SharedState n SharedKey) SharedKey
        -> Server SharedWallets
    sharedWallets apilayer =
             postSharedWallet @_ @_ @SharedKey apilayer Shared.generateKeyFromSeed SharedKey
        :<|> (fmap fst . getWallet apilayer mkSharedWallet)
        :<|> (fmap fst <$> listWallets apilayer mkSharedWallet)
        :<|> patchSharedWallet @_ @_ @SharedKey apilayer SharedKey Payment
        :<|> patchSharedWallet @_ @_ @SharedKey apilayer SharedKey Delegation
        :<|> deleteWallet apilayer

    sharedWalletKeys
        :: ApiLayer (SharedState n SharedKey) SharedKey
        -> Server SharedWalletKeys
    sharedWalletKeys apilayer = derivePublicKey apilayer ApiVerificationKeyShared
        :<|> (\wid ix p -> postAccountPublicKey apilayer ApiAccountKeyShared wid ix (toKeyDataPurpose p) )
        :<|> getAccountPublicKey apilayer ApiAccountKeyShared
      where
          toKeyDataPurpose :: ApiPostAccountKeyData -> ApiPostAccountKeyDataWithPurpose
          toKeyDataPurpose (ApiPostAccountKeyData p f) =
              ApiPostAccountKeyDataWithPurpose p f Nothing

    sharedAddresses
        :: ApiLayer (SharedState n SharedKey) SharedKey
        -> Server (SharedAddresses n)
    sharedAddresses apilayer =
             listAddresses apilayer (normalizeSharedAddress @_ @SharedKey @n)

postAnyAddress
    :: NetworkId
    -> ApiAddressData
    -> Either TextDecodingError AnyAddress
postAnyAddress net addrData = do
    (addr, addrType) <- case addrData of
        (ApiAddressData (AddrEnterprise spendingCred) validation') -> do
            guardValidation validation' spendingCred
            pure ( unAddress $
                     CA.paymentAddress discriminant (spendingFrom spendingCred)
                 , EnterpriseDelegating )
        (ApiAddressData (AddrRewardAccount stakingCred) validation') -> do
            let (Right stakeAddr) =
                    CA.stakeAddress discriminant (stakingFrom stakingCred)
            guardValidation validation' stakingCred
            pure ( unAddress stakeAddr, RewardAccount )
        (ApiAddressData (AddrBase spendingCred stakingCred) validation') -> do
            guardValidation validation' spendingCred
            guardValidation validation' stakingCred
            pure ( unAddress $ CA.delegationAddress discriminant
                     (spendingFrom spendingCred) (stakingFrom stakingCred)
                 , EnterpriseDelegating )
    pure $ AnyAddress addr addrType (fromInteger netTag)
  where
      toXPub = fromJust . CA.xpubFromBytes . pubToXPub
      pubToXPub bytes = BS.append bytes bytes
      netTag = case net of
          Bcc.Mainnet -> 1
          _ -> 0
      spendingFrom cred = case cred of
          CredentialPubKey  bytes ->
              CA.PaymentFromKey $ CA.liftXPub $ toXPub bytes
          CredentialScript  script' ->
              CA.PaymentFromScript $ CA.toScriptHash script'
      stakingFrom cred = case cred of
          CredentialPubKey bytes ->
              CA.DelegationFromKey $ CA.liftXPub $ toXPub bytes
          CredentialScript script' ->
              CA.DelegationFromScript $ CA.toScriptHash script'
      guardValidation v cred =
            when (fst $ checkValidation v cred) $
                Left $ snd $ checkValidation v cred
      checkValidation v cred = case cred of
          CredentialPubKey _ -> (False, TextDecodingError "")
          CredentialScript script' -> case v of
              Just (ApiT v') ->
                  case validateScript v' script' of
                      Left err -> (True, TextDecodingError $ prettyErrValidateScript err)
                      Right _ -> (False, TextDecodingError "")
              _ -> (False, TextDecodingError "")
      (Right discriminant) = CA.mkNetworkDiscriminant netTag
