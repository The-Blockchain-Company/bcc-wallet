{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- HLINT ignore "Use newtype instead of data" -}

module Bcc.Wallet.Api
    ( -- * API
      Api
    , ApiV2

      -- * Type Families
    , PostData

      -- * Sophie
    , Wallets
        , DeleteWallet
        , GetWallet
        , ListWallets
        , PostWallet
        , PutWallet
        , PutWalletPassphrase
        , GetUTxOsStatistics
        , GetWalletUtxoSnapshot

    , WalletKeys
        , GetWalletKey
        , SignMetadata
        , PostAccountKey
        , GetAccountKey

    , Assets
        , ListAssets
        , GetAsset
        , GetAssetDefault
        , MintBurnAssets

    , Addresses
        , ListAddresses
        , InspectAddress
        , PostAnyAddress

    , CoinSelections
        , SelectCoins

    , SophieTransactions
        , ConstructTransaction
        , SignTransaction
        , ListTransactions
        , GetTransaction
        , DeleteTransaction
        , CreateTransactionOld
        , PostTransactionFeeOld
        , BalanceTransaction

    , StakePools
        , ListStakePools
        , JoinStakePool
        , QuitStakePool
        , DelegationFee
        , ListStakeKeys
        , PostPoolMaintenance
        , GetPoolMaintenance

    , SophieMigrations
        , MigrateSophieWallet
        , CreateSophieWalletMigrationPlan

    -- * Settings
    , Settings
        , PutSettings
        , GetSettings

    -- * Cole
    , ColeWallets
        , DeleteColeWallet
        , GetColeWallet
        , ListColeWallets
        , PostColeWallet
        , PutColeWallet
        , GetColeUTxOsStatistics
        , GetColeWalletUtxoSnapshot
        , PutColeWalletPassphrase

    , ColeAssets
        , ListColeAssets
        , GetColeAsset
        , GetColeAssetDefault

    , ColeAddresses
        , PostColeAddress
        , PutColeAddress
        , PutColeAddresses
        , ListColeAddresses

    , ColeCoinSelections
        , ColeSelectCoins

    , ColeTransactions
        , ConstructColeTransaction
        , SignColeTransaction
        , ListColeTransactions
        , GetColeTransaction
        , DeleteColeTransaction
        , CreateColeTransactionOld
        , PostColeTransactionFeeOld

    , ColeMigrations
        , MigrateColeWallet
        , CreateColeWalletMigrationPlan

    -- * Miscellaneous
    , Network
        , GetNetworkInformation
        , GetNetworkParameters
        , GetNetworkClock
    , SMASH
        , GetCurrentSMASHHealth

      -- * Shared Wallets
    , SharedWallets
        , PostSharedWallet
        , GetSharedWallet
        , ListSharedWallets
        , PatchSharedWalletInPayment
        , PatchSharedWalletInDelegation
        , DeleteSharedWallet

    , SharedWalletKeys
        , GetSharedWalletKey
        , PostAccountKeyShared
        , GetAccountKeyShared

    , SharedAddresses
        , ListSharedAddresses

    , Proxy_
        , PostExternalTransaction

      -- * Api Layer
    , ApiLayer (..)
    , HasWorkerRegistry
    , workerRegistry
    , WalletLock (..)
    , walletLocks
    , HasDBFactory
    , dbFactory
    , tokenMetadataClient
    , HasTokenMetadataClient
    ) where

import Prelude

import Bcc.Wallet
    ( TxSubmitLog, WalletLayer (..), WalletWorkerLog )
import Bcc.Wallet.Api.Types
    ( AnyAddress
    , ApiAccountKey
    , ApiAccountKeyShared
    , ApiAddressData
    , ApiAddressIdT
    , ApiAddressInspect
    , ApiAddressInspectData
    , ApiAddressT
    , ApiAsset
    , ApiBalanceTransactionPostDataT
    , ApiColeWallet
    , ApiCoinSelectionT
    , ApiConstructTransactionDataT
    , ApiConstructTransactionT
    , ApiFee
    , ApiHealthCheck
    , ApiMaintenanceAction
    , ApiMaintenanceActionPostData
    , ApiMintedBurnedTransactionT
    , ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiPoolId
    , ApiPostAccountKeyData
    , ApiPostAccountKeyDataWithPurpose
    , ApiPostRandomAddressData
    , ApiPutAddressesDataT
    , ApiSelectCoinsDataT
    , ApiSharedWallet
    , ApiSharedWalletPatchData
    , ApiSharedWalletPostData
    , ApiSignTransactionPostData
    , ApiSignedTransaction
    , ApiStakeKeysT
    , ApiT
    , ApiTransactionT
    , ApiTxId
    , ApiUtxoStatistics
    , ApiVerificationKeyShared
    , ApiVerificationKeySophie
    , ApiWallet
    , ApiWalletMigrationPlan
    , ApiWalletMigrationPlanPostDataT
    , ApiWalletMigrationPostDataT
    , ApiWalletPassphrase
    , ApiWalletSignData
    , ApiWalletUtxoSnapshot
    , ColeWalletPutPassphraseData
    , Iso8601Time
    , KeyFormat
    , MinWithdrawal
    , PostMintBurnAssetDataT
    , PostTransactionFeeOldDataT
    , PostTransactionOldDataT
    , SettingsPutData
    , SomeColeWalletPostData
    , WalletOrAccountPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Bcc.Wallet.DB
    ( DBFactory, DBLayer )
import Bcc.Wallet.Network
    ( NetworkLayer )
import Bcc.Wallet.Primitive.AddressDerivation
    ( Depth, DerivationIndex, Role )
import Bcc.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Bcc.Wallet.Primitive.Types
    ( Block
    , NetworkParameters
    , SmashServer (..)
    , SortOrder (..)
    , WalletId (..)
    )
import Bcc.Wallet.Primitive.Types.Address
    ( AddressState )
import Bcc.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Bcc.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Bcc.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Bcc.Wallet.Registry
    ( HasWorkerCtx (..), WorkerLog, WorkerRegistry )
import Bcc.Wallet.TokenMetadata
    ( TokenMetadataClient )
import Bcc.Wallet.Transaction
    ( TransactionLayer )
import Control.Concurrent.Concierge
    ( Concierge )
import Control.Tracer
    ( Tracer, contramap )
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( Lens' )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Kind
    ( Type )
import Data.List.NonEmpty
    ( NonEmpty )
import GHC.Generics
    ( Generic )
import Servant.API
    ( (:<|>)
    , (:>)
    , Capture
    , JSON
    , OctetStream
    , QueryFlag
    , QueryParam
    , ReqBody
    )
import Servant.API.Verbs
    ( DeleteAccepted
    , DeleteNoContent
    , Get
    , Patch
    , Post
    , PostAccepted
    , PostCreated
    , PostNoContent
    , Put
    , PutAccepted
    , PutNoContent
    )

import qualified Bcc.Wallet.Primitive.Types as W

type ApiV2 n apiPool = "v2" :> Api n apiPool

-- | The full bcc-wallet API.
type Api n apiPool =
         Wallets
    :<|> WalletKeys
    :<|> Assets n
    :<|> Addresses n
    :<|> CoinSelections n
    :<|> SophieTransactions n
    :<|> SophieMigrations n
    :<|> StakePools n apiPool
    :<|> ColeWallets
    :<|> ColeAssets
    :<|> ColeAddresses n
    :<|> ColeCoinSelections n
    :<|> ColeTransactions n
    :<|> ColeMigrations n
    :<|> Network
    :<|> Proxy_
    :<|> Settings
    :<|> SMASH
    :<|> SharedWallets
    :<|> SharedWalletKeys
    :<|> SharedAddresses n

{-------------------------------------------------------------------------------
                                  Wallets

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Wallets
-------------------------------------------------------------------------------}

type Wallets =
    DeleteWallet
    :<|> GetWallet
    :<|> ListWallets
    :<|> PostWallet
    :<|> PutWallet
    :<|> PutWalletPassphrase
    :<|> GetWalletUtxoSnapshot
    :<|> GetUTxOsStatistics

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/deleteWallet
type DeleteWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getWallet
type GetWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listWallets
type ListWallets = "wallets"
    :> Get '[JSON] [ApiWallet]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postWallet
type PostWallet = "wallets"
    :> ReqBody '[JSON] (PostData ApiWallet)
    :> PostCreated '[JSON] ApiWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/putWallet
type PutWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] WalletPutData
    :> Put '[JSON] ApiWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/putWalletPassphrase
type PutWalletPassphrase = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "passphrase"
    :> ReqBody '[JSON] WalletPutPassphraseData
    :> PutNoContent

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getWalletUtxoSnapshot
type GetWalletUtxoSnapshot = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "utxo"
    :> Get '[JSON] ApiWalletUtxoSnapshot

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getUTxOsStatistics
type GetUTxOsStatistics = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

{-------------------------------------------------------------------------------
                                  Wallet Keys
  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Keys
-------------------------------------------------------------------------------}

type WalletKeys =
    GetWalletKey
    :<|> SignMetadata
    :<|> PostAccountKey
    :<|> GetAccountKey

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getWalletKey
type GetWalletKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiVerificationKeySophie

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/signMetadata
type SignMetadata = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "signatures"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiWalletSignData
    :> Post '[OctetStream] ByteString

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postAccountKey
type PostAccountKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiPostAccountKeyDataWithPurpose
    :> PostAccepted '[JSON] ApiAccountKey

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getAccountKey
type GetAccountKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> QueryParam "format" KeyFormat
    :> Get '[JSON] ApiAccountKey

{-------------------------------------------------------------------------------
                                  Assets

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Assets
-------------------------------------------------------------------------------}

type Assets n =
    MintBurnAssets n
    :<|> ListAssets
    :<|> GetAsset
    :<|> GetAssetDefault

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listAssets
type ListAssets = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Get '[JSON] [ApiAsset]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getAsset
type GetAsset = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Capture "assetName" (ApiT TokenName)
    :> Get '[JSON] ApiAsset

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getAssetDefault
type GetAssetDefault = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Get '[JSON] ApiAsset

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/mintBurnAssets
type MintBurnAssets n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> ReqBody '[JSON] (PostMintBurnAssetDataT n)
    :> PostAccepted '[JSON] (ApiMintedBurnedTransactionT n)

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Addresses
-------------------------------------------------------------------------------}

type Addresses n =
    ListAddresses n
    :<|> InspectAddress
    :<|> PostAnyAddress n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listAddresses
type ListAddresses n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/inspectAddress
type InspectAddress = "addresses"
    :> Capture "addressId" ApiAddressInspectData
    :> Get '[JSON] ApiAddressInspect

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postAnyAddress
type PostAnyAddress n = "addresses"
    :> ReqBody '[JSON] ApiAddressData
    :> PostAccepted '[JSON] AnyAddress

{-------------------------------------------------------------------------------
                               Coin Selections

  See also:
  https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Coin-Selections
-------------------------------------------------------------------------------}

type CoinSelections n =
    SelectCoins n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/selectCoins
type SelectCoins n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "coin-selections"
    :> "random"
    :> ReqBody '[JSON] (ApiSelectCoinsDataT n)
    :> Post '[JSON] (ApiCoinSelectionT n)

{-------------------------------------------------------------------------------
                                  SophieTransactions

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Transactions
-------------------------------------------------------------------------------}

type SophieTransactions n =
         ConstructTransaction n
    :<|> SignTransaction n
    :<|> ListTransactions n
    :<|> GetTransaction n
    :<|> DeleteTransaction
    :<|> CreateTransactionOld n
    :<|> PostTransactionFeeOld n
    :<|> BalanceTransaction n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/constructTransaction
type ConstructTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-construct"
    :> ReqBody '[JSON] (ApiConstructTransactionDataT n)
    :> PostAccepted '[JSON] (ApiConstructTransactionT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/signTransaction
type SignTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-sign"
    :> ReqBody '[JSON] ApiSignTransactionPostData
    :> PostAccepted '[JSON] ApiSignedTransaction

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postTransaction
type CreateTransactionOld n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionOldDataT n)
    :> PostAccepted '[JSON] (ApiTransactionT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listTransactions
type ListTransactions n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "minWithdrawal" MinWithdrawal
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> Get '[JSON] [ApiTransactionT n]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getTransaction
type GetTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> Get '[JSON] (ApiTransactionT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postTransactionFee
type PostTransactionFeeOld n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeOldDataT n)
    :> PostAccepted '[JSON] ApiFee

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/deleteTransaction
type DeleteTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/balanceTransaction
type BalanceTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-balance"
    :> ReqBody '[JSON] (ApiBalanceTransactionPostDataT n)
    :> PostAccepted '[JSON] (ApiConstructTransactionT n)

{-------------------------------------------------------------------------------
                                 Sophie Migrations

See also:
https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Migrations
-------------------------------------------------------------------------------}

type SophieMigrations n =
         CreateSophieWalletMigrationPlan n
    :<|> MigrateSophieWallet n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/migrateSophieWallet
type MigrateSophieWallet n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "raw")
    :> PostAccepted '[JSON] (NonEmpty (ApiTransactionT n))

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/createSophieWalletMigrationPlan
type CreateSophieWalletMigrationPlan n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> "plan"
    :> ReqBody '[JSON] (ApiWalletMigrationPlanPostDataT n)
    :> PostAccepted '[JSON] (ApiWalletMigrationPlan n)

{-------------------------------------------------------------------------------
                                  StakePools

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/edge/#tag/Stake-Pools
-------------------------------------------------------------------------------}

type StakePools n apiPool =
    ListStakePools apiPool
    :<|> JoinStakePool n
    :<|> QuitStakePool n
    :<|> DelegationFee
    :<|> ListStakeKeys n
    :<|> PostPoolMaintenance
    :<|> GetPoolMaintenance

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/edge/#operation/listStakePools
type ListStakePools apiPool = "stake-pools"
    :> QueryParam "stake" (ApiT Coin)
    :> Get '[JSON] [apiPool]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/joinStakePool
type JoinStakePool n = "stake-pools"
    :> Capture "stakePoolId" ApiPoolId
    :> "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> PutAccepted '[JSON] (ApiTransactionT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/quitStakePool
type QuitStakePool n = "stake-pools"
    :> "*"
    :> "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> DeleteAccepted '[JSON] (ApiTransactionT n)

type ListStakeKeys n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "stake-keys"
    :> Get '[JSON] (ApiStakeKeysT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getDelegationFee
type DelegationFee = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "delegation-fees"
    :> Get '[JSON] ApiFee

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postPoolMaintenance
type PostPoolMaintenance = "stake-pools"
    :> "maintenance-actions"
    :> ReqBody '[JSON] ApiMaintenanceActionPostData
    :> PostNoContent

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getPoolMaintenance
type GetPoolMaintenance = "stake-pools"
    :> "maintenance-actions"
    :> Get '[JSON] ApiMaintenanceAction

{-------------------------------------------------------------------------------
                                  Settings
-------------------------------------------------------------------------------}

type Settings = PutSettings :<|> GetSettings

type PutSettings = "settings"
    :> ReqBody '[JSON] SettingsPutData
    :> PutNoContent

type GetSettings = "settings"
    :> Get '[JSON] (ApiT W.Settings)

{-------------------------------------------------------------------------------
                                 Cole Wallets

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Cole-Wallets
-------------------------------------------------------------------------------}

type ColeWallets =
         PostColeWallet
    :<|> DeleteColeWallet
    :<|> GetColeWallet
    :<|> ListColeWallets
    :<|> PutColeWallet
    :<|> GetColeWalletUtxoSnapshot
    :<|> GetColeUTxOsStatistics
    :<|> PutColeWalletPassphrase

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postColeWallet
type PostColeWallet = "cole-wallets"
    :> ReqBody '[JSON] (PostData ApiColeWallet)
    :> PostCreated '[JSON] ApiColeWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/deleteColeWallet
type DeleteColeWallet = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getColeWallet
type GetColeWallet = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiColeWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listColeWallets
type ListColeWallets = "cole-wallets"
    :> Get '[JSON] [ApiColeWallet]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/putColeWallet
type PutColeWallet = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] WalletPutData
    :> Put '[JSON] ApiColeWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getColeWalletUtxoSnapshot
type GetColeWalletUtxoSnapshot = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "utxo"
    :> Get '[JSON] ApiWalletUtxoSnapshot

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getColeUTxOsStatistics
type GetColeUTxOsStatistics = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/putColeWalletPassphrase
type PutColeWalletPassphrase = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "passphrase"
    :> ReqBody '[JSON] ColeWalletPutPassphraseData
    :> PutNoContent

{-------------------------------------------------------------------------------
                                  Assets

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/ColeAssets
-------------------------------------------------------------------------------}

type ColeAssets =
    ListColeAssets
    :<|> GetColeAsset
    :<|> GetColeAssetDefault

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listColeAssets
type ListColeAssets = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Get '[JSON] [ApiAsset]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getColeAsset
type GetColeAsset = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Capture "assetName" (ApiT TokenName)
    :> Get '[JSON] ApiAsset

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getColeAssetDefault
type GetColeAssetDefault = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Get '[JSON] ApiAsset

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Cole-Addresses
-------------------------------------------------------------------------------}

type ColeAddresses n =
    PostColeAddress n
    :<|> PutColeAddress n
    :<|> PutColeAddresses n
    :<|> ListColeAddresses n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/createAddress
type PostColeAddress n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> ReqBody '[JSON] ApiPostRandomAddressData
    :> PostCreated '[JSON] (ApiAddressT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/restoreAddress
type PutColeAddress n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> Capture "addressId" (ApiAddressIdT n)
    :> PutNoContent

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/importAddresses
type PutColeAddresses n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> ReqBody '[JSON] (ApiPutAddressesDataT n)
    :> PutNoContent

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listColeAddresses
type ListColeAddresses n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

{-------------------------------------------------------------------------------
                               Coin Selections

  See also:
  https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Cole-Coin-Selections
-------------------------------------------------------------------------------}

type ColeCoinSelections n =
    ColeSelectCoins n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/coleSelectCoins
type ColeSelectCoins n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "coin-selections"
    :> "random"
    :> ReqBody '[JSON] (ApiSelectCoinsDataT n)
    :> Post '[JSON] (ApiCoinSelectionT n)

{-------------------------------------------------------------------------------
                                 Cole Transactions

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Cole-Transactions
-------------------------------------------------------------------------------}

type ColeTransactions n =
         ConstructColeTransaction n
    :<|> SignColeTransaction n
    :<|> ListColeTransactions n
    :<|> GetColeTransaction n
    :<|> DeleteColeTransaction
    :<|> CreateColeTransactionOld n
    :<|> PostColeTransactionFeeOld n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/constructColeTransaction
type ConstructColeTransaction n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-construct"
    :> ReqBody '[JSON] (ApiConstructTransactionDataT n)
    :> PostAccepted '[JSON] (ApiConstructTransactionT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/signColeTransaction
type SignColeTransaction n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-sign"
    :> ReqBody '[JSON] ApiSignTransactionPostData
    :> PostAccepted '[JSON] ApiSignedTransaction

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postColeTransaction
type CreateColeTransactionOld n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionOldDataT n)
    :> PostAccepted '[JSON] (ApiTransactionT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listColeTransactions
type ListColeTransactions n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> Get '[JSON] [ApiTransactionT n]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getColeTransaction
type GetColeTransaction n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> Get '[JSON] (ApiTransactionT n)

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postColeTransactionFee
type PostColeTransactionFeeOld n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeOldDataT n)
    :> PostAccepted '[JSON] ApiFee

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/deleteColeTransaction
type DeleteColeTransaction = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent

{-------------------------------------------------------------------------------
                                 Cole Migrations

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Cole-Migrations
-------------------------------------------------------------------------------}

type ColeMigrations n =
         CreateColeWalletMigrationPlan n
    :<|> MigrateColeWallet n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/migrateColeWallet
type MigrateColeWallet n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "lenient")
    :> PostAccepted '[JSON] (NonEmpty (ApiTransactionT n))

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/createColeWalletMigrationPlan
type CreateColeWalletMigrationPlan n = "cole-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> "plan"
    :> ReqBody '[JSON] (ApiWalletMigrationPlanPostDataT n)
    :> PostAccepted '[JSON] (ApiWalletMigrationPlan n)

{-------------------------------------------------------------------------------
                                  Network

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Network
-------------------------------------------------------------------------------}

type Network =
         GetNetworkInformation
    :<|> GetNetworkParameters
    :<|> GetNetworkClock

type GetNetworkInformation = "network"
    :> "information"
    :> Get '[JSON] ApiNetworkInformation

type GetNetworkParameters = "network"
    :> "parameters"
    :> Get '[JSON] ApiNetworkParameters

type GetNetworkClock = "network"
    :> "clock"
    :> QueryFlag "forceNtpCheck"
    :> Get '[JSON] ApiNetworkClock

{-------------------------------------------------------------------------------
                                  SMASH

-------------------------------------------------------------------------------}

type SMASH = GetCurrentSMASHHealth

type GetCurrentSMASHHealth = "smash"
    :> "health"
    :> QueryParam "url" (ApiT SmashServer)
    :> Get '[JSON] ApiHealthCheck

{-------------------------------------------------------------------------------
                                  Shared Wallets

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Shared-Wallets
-------------------------------------------------------------------------------}

type SharedWallets =
         PostSharedWallet
    :<|> GetSharedWallet
    :<|> ListSharedWallets
    :<|> PatchSharedWalletInPayment
    :<|> PatchSharedWalletInDelegation
    :<|> DeleteSharedWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postSharedWallet
type PostSharedWallet = "shared-wallets"
    :> ReqBody '[JSON] ApiSharedWalletPostData
    :> PostCreated '[JSON] ApiSharedWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getSharedWallet
type GetSharedWallet = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiSharedWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listSharedWallets
type ListSharedWallets = "shared-wallets"
    :> Get '[JSON] [ApiSharedWallet]

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/patchSharedWalletInPayment
type PatchSharedWalletInPayment = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-script-template"
    :> ReqBody '[JSON] ApiSharedWalletPatchData
    :> Patch '[JSON] ApiSharedWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/patchSharedWalletInDelegation
type PatchSharedWalletInDelegation = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "delegation-script-template"
    :> ReqBody '[JSON] ApiSharedWalletPatchData
    :> Patch '[JSON] ApiSharedWallet

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/deleteSharedWallet
type DeleteSharedWallet = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

{-------------------------------------------------------------------------------
                                  Shared Wallet Keys
  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Keys
-------------------------------------------------------------------------------}

type SharedWalletKeys =
         GetSharedWalletKey
    :<|> PostAccountKeyShared
    :<|> GetAccountKeyShared

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getSharedWalletKey
type GetSharedWalletKey = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiVerificationKeyShared

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postAccountKeyShared
type PostAccountKeyShared = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiPostAccountKeyData
    :> PostAccepted '[JSON] ApiAccountKeyShared

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/getAccountKeyShared
type GetAccountKeyShared = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> QueryParam "format" KeyFormat
    :> Get '[JSON] ApiAccountKeyShared

{-------------------------------------------------------------------------------
                                 Shared Addresses

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/#tag/Shared-Addresses
-------------------------------------------------------------------------------}

type SharedAddresses n =
    ListSharedAddresses n

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/listSharedAddresses
type ListSharedAddresses n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

{-------------------------------------------------------------------------------
                                   Proxy_

  See also: https://The-Blockchain-Company.github.io/bcc-wallet/api/edge/#tag/Proxy
-------------------------------------------------------------------------------}

type Proxy_ =
    PostExternalTransaction

-- | https://The-Blockchain-Company.github.io/bcc-wallet/api/#operation/postExternalTransaction
type PostExternalTransaction = "proxy"
    :> "transactions"
    :> ReqBody '[OctetStream] (ApiT SealedTx)
    :> PostAccepted '[JSON] ApiTxId

{-------------------------------------------------------------------------------
                               Api Layer
-------------------------------------------------------------------------------}

data ApiLayer s (k :: Depth -> Type -> Type)
    = ApiLayer
        (Tracer IO TxSubmitLog)
        (Tracer IO (WorkerLog WalletId WalletWorkerLog))
        (Block, NetworkParameters, SyncTolerance)
        (NetworkLayer IO (Block))
        (TransactionLayer k SealedTx)
        (DBFactory IO s k)
        (WorkerRegistry WalletId (DBLayer IO s k))
        (Concierge IO WalletLock)
        (TokenMetadataClient IO)
    deriving (Generic)

-- | Locks that are held by the wallet in order to enforce
-- sequential executation of some API actions.
-- Used with "Control.Concurrent.Concierge".
data WalletLock = PostTransactionOld WalletId
    deriving (Eq, Ord, Show)

instance HasWorkerCtx (DBLayer IO s k) (ApiLayer s k) where
    type WorkerCtx (ApiLayer s k) = WalletLayer IO s k
    type WorkerMsg (ApiLayer s k) = WalletWorkerLog
    type WorkerKey (ApiLayer s k) = WalletId
    hoistResource db transform (ApiLayer _ tr gp nw tl _ _ _ _) =
        WalletLayer (contramap transform tr) gp nw tl db

{-------------------------------------------------------------------------------
                               Capabilities
-------------------------------------------------------------------------------}

type HasWorkerRegistry s k ctx =
    ( HasType (WorkerRegistry WalletId (DBLayer IO s k)) ctx
    , HasWorkerCtx (DBLayer IO s k) ctx
    , WorkerKey ctx ~ WalletId
    , WorkerMsg ctx ~ WalletWorkerLog
    )

workerRegistry
    :: forall s k ctx. (HasWorkerRegistry s k ctx)
    => Lens' ctx (WorkerRegistry WalletId (DBLayer IO s k))
workerRegistry =
    typed @(WorkerRegistry WalletId (DBLayer IO s k))

type HasDBFactory s k = HasType (DBFactory IO s k)
type HasTokenMetadataClient = HasType (TokenMetadataClient IO)

dbFactory
    :: forall s k ctx. (HasDBFactory s k ctx)
    => Lens' ctx (DBFactory IO s k)
dbFactory =
    typed @(DBFactory IO s k)

tokenMetadataClient
    :: forall ctx. (HasTokenMetadataClient ctx)
    => Lens' ctx (TokenMetadataClient IO)
tokenMetadataClient =
    typed @(TokenMetadataClient IO)

walletLocks
    :: forall ctx. (HasType (Concierge IO WalletLock) ctx)
    => Lens' ctx (Concierge IO WalletLock)
walletLocks =
    typed @(Concierge IO WalletLock)

{-------------------------------------------------------------------------------
                              Type Families
-------------------------------------------------------------------------------}

type family PostData wallet :: Type where
    PostData ApiWallet = WalletOrAccountPostData
    PostData ApiColeWallet = SomeColeWalletPostData
