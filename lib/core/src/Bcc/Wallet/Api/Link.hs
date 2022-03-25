{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2021-2022 TBCO
-- License: Apache-2.0
--
-- Type-safe endpoint accessors for the wallet API. Under normal circumstances,
-- one would prefer to use 'WalletClient' from 'Bcc.Wallet.Api.Client' and
-- not to bother with endpoints at all.
--
-- Yet, in some cases (like in black-box testing), one could want to purposely
-- send malformed requests to specific endpoints. Thus, this module facilitates
-- the construction of valid endpoints that'd be accepted by the server, and for
-- which, users are free to send all sort of data as payload, valid or invalid.
--
-- This module is meant to be used via qualified imports and with
-- type-applications since all exposed functions are type ambiguous in a
-- variable @style@ of type 'WalletStyle'.
--
-- @import qualified Bcc.Wallet.Api.Link as Link@
--
-- For examples:
--
-- >>> Link.deleteWallet @'Sophie myWallet
-- ( "DELETE", "/v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
--
-- >>> Link.getWallet @('Cole 'Icarus) myWallet
-- ( "GET", "/v2/cole-wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )

module Bcc.Wallet.Api.Link
    ( -- * Wallets
      deleteWallet
    , getWallet
    , listWallets
    , postWallet
    , putWallet
    , putWalletPassphrase
    , getWalletUtxoSnapshot
    , getUTxOsStatistics
    , createMigrationPlan
    , migrateWallet

     -- * WalletKeys
    , getWalletKey
    , signMetadata
    , postAccountKey
    , getAccountKey

      -- * Addresses
    , postRandomAddress
    , putRandomAddresses
    , listAddresses
    , listAddresses'
    , inspectAddress
    , postAnyAddress

      -- * CoinSelections
    , selectCoins

      -- * Assets
    , listAssets
    , getAsset
    , listColeAssets
    , getColeAsset
    , mintBurnAssets

      -- * Transactions
    , createTransactionOld
    , listTransactions
    , listTransactions'
    , getTransactionFeeOld
    , deleteTransaction
    , getTransaction
    , createUnsignedTransaction
    , signTransaction
    , balanceTransaction

      -- * StakePools
    , listStakePools
    , listStakeKeys
    , joinStakePool
    , quitStakePool
    , getDelegationFee
    , postPoolMaintenance
    , getPoolMaintenance

      -- * Network
    , getNetworkInfo
    , getNetworkParams
    , getNetworkClock
    , getNetworkClock'

      -- * Proxy
    , postExternalTransaction

      -- * Settings
    , putSettings
    , getSettings

      -- * Utils

    , getCurrentSMASHHealth

     -- * Shared Wallets
    , patchSharedWallet

    , PostWallet
    , Discriminate
    ) where

import Prelude

import Bcc.Wallet.Api.Types
    ( ApiAddressInspectData (..)
    , ApiPoolId (..)
    , ApiT (..)
    , ApiTxId (ApiTxId)
    , Iso8601Time
    , KeyFormat
    , MinWithdrawal (..)
    , WalletStyle (..)
    )
import Bcc.Wallet.Primitive.AddressDerivation
    ( DerivationIndex, NetworkDiscriminant (..), Role )
import Bcc.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..) )
import Bcc.Wallet.Primitive.Types
    ( PoolId, SmashServer, SortOrder, WalletId (..) )
import Bcc.Wallet.Primitive.Types.Address
    ( AddressState )
import Bcc.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Bcc.Wallet.Primitive.Types.Hash
    ( Hash )
import Bcc.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId, nullTokenName )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( Symbol )
import Network.HTTP.Types.Method
    ( Method )
import Numeric.Natural
    ( Natural )
import Servant.API
    ( (:>)
    , Capture
    , Header'
    , IsElem
    , NoContentVerb
    , QueryFlag
    , QueryParam
    , ReflectMethod (..)
    , ReqBody
    , Verb
    )
import Servant.Links
    ( HasLink (..), safeLink' )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Bcc.Wallet.Api as Api

--
-- Wallets
--

-- NOTE
-- Type-class is necessary here to type-check 'IsElem endpoint Api' below.
class PostWallet (style :: WalletStyle) where
    postWallet :: (Method, Text)

instance PostWallet 'Sophie where
    postWallet = endpoint @Api.PostWallet id

instance PostWallet 'Cole where
    postWallet = endpoint @Api.PostColeWallet id

instance PostWallet 'Shared where
    postWallet = endpoint @Api.PostSharedWallet id

deleteWallet
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
deleteWallet w = discriminate @style
    (endpoint @Api.DeleteWallet (wid &))
    (endpoint @Api.DeleteColeWallet (wid &))
    (endpoint @Api.DeleteSharedWallet (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

getWallet
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getWallet w = discriminate @style
    (endpoint @Api.GetWallet (wid &))
    (endpoint @Api.GetColeWallet (wid &))
    (endpoint @Api.GetSharedWallet (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)

getUTxOsStatistics
    :: forall (style :: WalletStyle) w.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getUTxOsStatistics w = discriminate @style
    (endpoint @Api.GetUTxOsStatistics (wid &))
    (endpoint @Api.GetColeUTxOsStatistics (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

getWalletUtxoSnapshot
    :: forall (style :: WalletStyle) w.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getWalletUtxoSnapshot w = discriminate @style
    (endpoint @Api.GetWalletUtxoSnapshot (wid &))
    (endpoint @Api.GetColeWalletUtxoSnapshot (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

listWallets
    :: forall (style :: WalletStyle).
        ( Discriminate style
        )
    => (Method, Text)
listWallets = discriminate @style
    (endpoint @Api.ListWallets id)
    (endpoint @Api.ListColeWallets id)
    (endpoint @Api.ListSharedWallets id)

putWallet
    :: forall (style :: WalletStyle) w.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
putWallet w = discriminate @style
    (endpoint @Api.PutWallet (wid &))
    (endpoint @Api.PutColeWallet (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

putWalletPassphrase
    :: forall (style :: WalletStyle) w.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
putWalletPassphrase w = discriminate @style
    (endpoint @Api.PutWalletPassphrase (wid &))
    (endpoint @Api.PutColeWalletPassphrase (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

migrateWallet
    :: forall (style :: WalletStyle) w.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
migrateWallet w = discriminate @style
    (endpoint @(Api.MigrateSophieWallet Net) (wid &))
    (endpoint @(Api.MigrateColeWallet Net) (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

createMigrationPlan
    :: forall (style :: WalletStyle) w.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
createMigrationPlan w = discriminate @style
    (endpoint @(Api.CreateSophieWalletMigrationPlan Net) (wid &))
    (endpoint @(Api.CreateColeWalletMigrationPlan Net) (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- WalletKeys
--

getWalletKey
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> Role
    -> DerivationIndex
    -> Maybe Bool
    -> (Method, Text)
getWalletKey w role_ index hashed = discriminate @style
    (endpoint @Api.GetWalletKey (\mk -> mk wid (ApiT role_) (ApiT index) hashed))
    (notSupported "Cole")
    (endpoint @Api.GetSharedWalletKey (\mk -> mk wid (ApiT role_) (ApiT index) hashed))
  where
    wid = w ^. typed @(ApiT WalletId)

signMetadata
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> Role
    -> DerivationIndex
    -> (Method, Text)
signMetadata w role_ index =
    endpoint @Api.SignMetadata (\mk -> mk wid (ApiT role_) (ApiT index))
  where
    wid = w ^. typed @(ApiT WalletId)

postAccountKey
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> DerivationIndex
    -> (Method, Text)
postAccountKey w index = discriminate @style
    (endpoint @Api.PostAccountKey (\mk -> mk wid (ApiT index)))
    (notSupported "Cole")
    (endpoint @Api.PostAccountKeyShared (\mk -> mk wid (ApiT index)))
  where
    wid = w ^. typed @(ApiT WalletId)

getAccountKey
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> Maybe KeyFormat
    -> (Method, Text)
getAccountKey w extended = discriminate @style
    (endpoint @Api.GetAccountKey (\mk -> mk wid extended))
    (notSupported "Cole")
    (endpoint @Api.GetAccountKeyShared (\mk -> mk wid extended))
  where
    wid = w ^. typed @(ApiT WalletId)


--
-- Addresses
--

postRandomAddress
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
postRandomAddress w =
    endpoint @(Api.PostColeAddress Net) (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

putRandomAddresses
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
putRandomAddresses w =
    endpoint @(Api.PutColeAddresses Net) (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

listAddresses
    :: forall style w.
        ( HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
listAddresses w =
    listAddresses' @style w Nothing

listAddresses'
    :: forall style w.
        ( HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> Maybe AddressState
    -> (Method, Text)
listAddresses' w mstate = discriminate @style
    (endpoint @(Api.ListAddresses Net) (\mk -> mk wid (ApiT <$> mstate)))
    (endpoint @(Api.ListColeAddresses Net) (\mk -> mk wid (ApiT <$> mstate)))
    (endpoint @(Api.ListSharedAddresses Net) (\mk -> mk wid (ApiT <$> mstate)))
  where
    wid = w ^. typed @(ApiT WalletId)

inspectAddress
    :: ApiAddressInspectData
    -> (Method, Text)
inspectAddress addr =
    endpoint @Api.InspectAddress (addr &)

postAnyAddress
    :: (Method, Text)
postAnyAddress =
    endpoint @(Api.PostAnyAddress Net) id

--
-- Coin Selections
--

selectCoins
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
selectCoins w = discriminate @style
    (endpoint @(Api.SelectCoins Net) (wid &))
    (endpoint @(Api.ColeSelectCoins Net) (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Assets
--

listAssets
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
listAssets w =
    endpoint @Api.ListAssets (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

getAsset
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> TokenPolicyId
    -> TokenName
    -> (Method, Text)
getAsset w pid n
    | n == nullTokenName = endpoint @Api.GetAssetDefault mkURLDefault
    | otherwise = endpoint @Api.GetAsset mkURL
  where
    wid = w ^. typed @(ApiT WalletId)
    mkURL mk = mk wid (ApiT pid) (ApiT n)
    mkURLDefault mk = mk wid (ApiT pid)

listColeAssets
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
listColeAssets w =
    endpoint @Api.ListColeAssets (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

getColeAsset
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> TokenPolicyId
    -> TokenName
    -> (Method, Text)
getColeAsset w pid n
    | n == nullTokenName = endpoint @Api.GetColeAssetDefault mkURLDefault
    | otherwise = endpoint @Api.GetColeAsset mkURL
  where
    wid = w ^. typed @(ApiT WalletId)
    mkURL mk = mk wid (ApiT pid) (ApiT n)
    mkURLDefault mk = mk wid (ApiT pid)

--
-- Transactions
--

createTransactionOld
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
createTransactionOld w = discriminate @style
    (endpoint @(Api.CreateTransactionOld Net) (wid &))
    (endpoint @(Api.CreateColeTransactionOld Net) (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

listTransactions
    :: forall (style :: WalletStyle) w.
        ( Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
listTransactions w =
    listTransactions' @style w Nothing Nothing Nothing Nothing

listTransactions'
    :: forall (style :: WalletStyle) w.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        )
    => w
    -> Maybe Natural
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe SortOrder
    -> (Method, Text)
listTransactions' w minWithdrawal inf sup order = discriminate @style
    (endpoint @(Api.ListTransactions Net)
        (\mk -> mk wid (MinWithdrawal <$> minWithdrawal) inf sup (ApiT <$> order)))
    (endpoint @(Api.ListColeTransactions Net)
        (\mk -> mk wid inf sup (ApiT <$> order)))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

getTransactionFeeOld
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
getTransactionFeeOld w = discriminate @style
    (endpoint @(Api.PostTransactionFeeOld Net) (wid &))
    (endpoint @(Api.PostColeTransactionFeeOld Net) (wid &))
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

deleteTransaction
    :: forall (style :: WalletStyle) w t.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        , HasType (ApiT (Hash "Tx")) t
        )
    => w
    -> t
    -> (Method, Text)
deleteTransaction w t = discriminate @style
    (endpoint @Api.DeleteTransaction mkURL)
    (endpoint @Api.DeleteColeTransaction mkURL)
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)
    tid = ApiTxId (t ^. typed @(ApiT (Hash "Tx")))
    mkURL mk = mk wid tid

getTransaction
    :: forall (style :: WalletStyle) w t.
        ( HasCallStack
        , Discriminate style
        , HasType (ApiT WalletId) w
        , HasType (ApiT (Hash "Tx")) t
        )
    => w
    -> t
    -> (Method, Text)
getTransaction w t = discriminate @style
    (endpoint @(Api.GetTransaction Net) mkURL)
    (endpoint @(Api.GetColeTransaction Net) mkURL)
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)
    tid = ApiTxId (t ^. typed @(ApiT (Hash "Tx")))
    mkURL mk = mk wid tid

createUnsignedTransaction
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
createUnsignedTransaction w = discriminate @style
    (endpoint @(Api.ConstructTransaction Net) (wid &))
    (endpoint @(Api.ConstructColeTransaction Net) (wid &))
    (notSupported "Shared") -- TODO: [ADP-909] should be supported in the final version of Transaction Workflow.
  where
    wid = w ^. typed @(ApiT WalletId)

signTransaction
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
signTransaction w = discriminate @style
    (endpoint @(Api.SignTransaction Net) (wid &))
    (endpoint @(Api.SignColeTransaction Net) (wid &))
    (notSupported "Shared") -- TODO: [ADP-909] should be supported in the final version of Transaction Workflow.
  where
    wid = w ^. typed @(ApiT WalletId)

balanceTransaction
    :: forall style w.
        ( HasCallStack
        , HasType (ApiT WalletId) w
        , Discriminate style
        )
    => w
    -> (Method, Text)
balanceTransaction w = discriminate @style
    (endpoint @(Api.BalanceTransaction Net) (wid &))
    (notSupported "Cole")
    (notSupported "Shared")
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Stake Pools
--
postPoolMaintenance
    :: (Method, Text)
postPoolMaintenance =
    endpoint @Api.PostPoolMaintenance id

getPoolMaintenance
    :: (Method, Text)
getPoolMaintenance =
    endpoint @Api.GetPoolMaintenance id

listStakePools
    :: Maybe Coin
    -> (Method, Text)
listStakePools stake =
    endpoint @(Api.ListStakePools ()) (\mk -> mk (ApiT <$> stake))

listStakeKeys
    :: forall w. (HasType (ApiT WalletId) w)
    => w
    -> (Method, Text)
listStakeKeys w =
    endpoint @(Api.ListStakeKeys ()) (\mk -> mk wid)
  where
    wid = w ^. typed @(ApiT WalletId)

joinStakePool
    :: forall s w.
        ( HasType (ApiT PoolId) s
        , HasType (ApiT WalletId) w
        )
    => s
    -> w
    -> (Method, Text)
joinStakePool s w =
    endpoint @(Api.JoinStakePool Net) (\mk -> mk sid wid)
  where
    sid = ApiPoolId $ getApiT $ s ^. typed @(ApiT PoolId)
    wid = w ^. typed @(ApiT WalletId)

quitStakePool
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
quitStakePool w =
    endpoint @(Api.QuitStakePool Net) (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

getDelegationFee
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
getDelegationFee w =
    endpoint @Api.DelegationFee (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Network Information
--

getNetworkInfo
    :: (Method, Text)
getNetworkInfo =
    endpoint @Api.GetNetworkInformation id

getNetworkParams
    :: (Method, Text)
getNetworkParams =
    endpoint @Api.GetNetworkParameters id

getNetworkClock
    :: (Method, Text)
getNetworkClock =
    endpoint @Api.GetNetworkClock (False &)

getNetworkClock'
    :: Bool -- ^ When 'True', block and force NTP check
    -> (Method, Text)
getNetworkClock' forceNtpCheck =
    endpoint @Api.GetNetworkClock (forceNtpCheck &)


--
-- Proxy
--

postExternalTransaction
    :: (Method, Text)
postExternalTransaction =
    endpoint @Api.PostExternalTransaction id

--
-- Settings
--

putSettings
    :: (Method, Text)
putSettings =
    endpoint @Api.PutSettings id

getSettings
    :: (Method, Text)
getSettings =
    endpoint @Api.GetSettings id

--
-- Utils
--
getCurrentSMASHHealth
    :: (Method, Text)
getCurrentSMASHHealth = getCurrentSMASHHealth' Nothing

getCurrentSMASHHealth'
    :: Maybe SmashServer
    -> (Method, Text)
getCurrentSMASHHealth' smash =
    endpoint @Api.GetCurrentSMASHHealth (\mk -> mk (ApiT <$> smash))

--
-- Shared Wallets
--
patchSharedWallet
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> CredentialType
    -> (Method, Text)
patchSharedWallet w cred =
    case cred of
        Payment ->
            endpoint @Api.PatchSharedWalletInPayment (wid &)
        Delegation ->
            endpoint @Api.PatchSharedWalletInDelegation (wid &)
  where
    wid = w ^. typed @(ApiT WalletId)

--
-- Internals
--

-- | A safe endpoint creator. This extracts the endpoint from a given Api type
-- in the form of:
--
-- - A 'Text' string (the URL)
-- - An HTTP 'Method' for calling this endpoint (e.g. GET, POST, ...)
--
-- This function does not type-check if the given endpoint (via type
-- application) is not actually part of the wallet API.
--
-- Note that the 'MkLink endpoint Text' depends on the type of 'endpoint'.
--
-- - For simple endpoints with no path or query parameters we have:
--
--   @MkLink endpoint Text ~ Text@ and therefore, we can simply do
--
--   >>> endpoint @Api.ListWallets id
--   ( "GET", "v2/wallets" )
--
-- - For endpoints with parameters, 'MkLink endpoint Text' will be a function
--   taking as many arguments as there are path or query parameters (path
--   parameters going first).
--
--   >>> endpoint @Api.GetWallet (\(mk :: ApiT WalletId -> Text) -> mk wid)
--   ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
--
--   Or, simply:
--
--   >>> endpoint @Api.GetWallet (wid &)
--   ( "GET", "v2/wallets/2512a00e9653fe49a44a5886202e24d77eeb998f" )
endpoint
    :: forall endpoint.
        ( HasLink endpoint
        , IsElem endpoint endpoint
        , HasVerb endpoint
        )
    => (MkLink endpoint Text -> Text)
    -> (Method, Text)
endpoint mk =
    ( method (Proxy @endpoint)
    , "v2/" <> mk (safeLink' toUrlPiece (Proxy @endpoint) (Proxy @endpoint))
    )

-- Returns first argument for Sophie style wallet, second argument otherwise.
class Discriminate (style :: WalletStyle) where
    discriminate :: a -> a -> a -> a

instance Discriminate 'Sophie where
    discriminate a _ _ = a

instance Discriminate 'Cole where
    discriminate _ a _ = a

instance Discriminate 'Shared where
    discriminate _ _ a = a

notSupported :: HasCallStack => String -> a
notSupported style = error $ "Endpoint not supported for " <> style <> " style"

-- | Some endpoints are parameterized via a network discriminant in order to
-- correctly encode their end type (for example, 'CreateTransaction n'). Yet, in
-- the context of this module, the network discrimination doesn't matter for it
-- has no influence on the endpoint's value and/or path parameters.
--
-- To ease type signatures, we therefore arbitrarily fix the network to Mainnet.
type Net = 'Mainnet

-- | Extract the method from a given Api
class HasVerb api where
    method :: Proxy api -> Method

instance (ReflectMethod m) => HasVerb (NoContentVerb m) where
    method _ = reflectMethod (Proxy @m)

instance (ReflectMethod m) => HasVerb (Verb m s ct a) where
    method _ = reflectMethod (Proxy @m)

instance HasVerb sub => HasVerb ((path :: Symbol) :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (Capture param t :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (ReqBody a b :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (QueryParam a b :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (QueryFlag sym :> sub) where
    method _ = method (Proxy @sub)

instance HasVerb sub => HasVerb (Header' opts name ty :> sub) where
    method _ = method (Proxy @sub)

mintBurnAssets
    :: forall w.
        ( HasType (ApiT WalletId) w
        )
    => w
    -> (Method, Text)
mintBurnAssets w = (endpoint @(Api.MintBurnAssets Net) (wid &))
  where
    wid = w ^. typed @(ApiT WalletId)
