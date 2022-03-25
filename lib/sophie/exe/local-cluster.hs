{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Prelude

import Bcc.BM.Data.Severity
    ( Severity (..) )
import Bcc.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Bcc.BM.Plugin
    ( loadPlugin )
import Bcc.CLI
    ( LogOutput (..)
    , Port
    , ekgEnabled
    , getEKGURL
    , getPrometheusURL
    , withLoggingNamed
    )
import Bcc.Startup
    ( installSignalHandlers, setDefaultFilePermissions, withUtf8Encoding )
import Bcc.Wallet.Api.Types
    ( EncodeAddress (..) )
import Bcc.Wallet.Logging
    ( stdoutTextTracer, trMessageText )
import Bcc.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Bcc.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Bcc.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Bcc.Wallet.Sophie
    ( SomeNetworkDiscriminant (..)
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Bcc.Wallet.Sophie.Launch
    ( withSystemTempDir )
import Bcc.Wallet.Sophie.Launch.Cluster
    ( ClusterLog (..)
    , RunningNode (..)
    , localClusterConfigFromEnv
    , moveInstantaneousRewardsTo
    , oneMillionBcc
    , sendFaucetAssetsTo
    , sendFaucetFundsTo
    , testMinSeverityFromEnv
    , tokenMetadataServerFromEnv
    , walletListenFromEnv
    , walletMinSeverityFromEnv
    , withCluster
    )
import Control.Arrow
    ( first )
import Control.Monad
    ( void, when )
import Control.Tracer
    ( contramap, traceWith )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( (</>) )
import Test.Integration.Faucet
    ( genRewardAccounts
    , jenIntegrationTestAssets
    , mirMnemonics
    , sophieIntegrationTestFunds
    )

import qualified Bcc.BM.Backend.EKGView as EKG
import qualified Data.Text as T

-- |
-- # OVERVIEW
--
-- This starts a cluster of Bcc nodes with:
--
-- - 1 relay node
-- - 1 BFT leader
-- - 4 stake pools
--
-- The BFT leader and pools are all fully connected. The network starts in the
-- Cole Era and transitions into the Sophie era. Once in the Sophie era and
-- once pools are registered and up-and-running, an instance of bcc-wallet
-- is started.
--
-- Pools have slightly different settings summarized in the table below:
--
-- | #       | Pledge | Retirement      | Metadata       |
-- | ---     | ---    | ---             | ---            |
-- | Pool #0 | 2M Bcc | Never           | Genesis Pool A |
-- | Pool #1 | 1M Bcc | Epoch 3         | Genesis Pool B |
-- | Pool #2 | 1M Bcc | Epoch 100_000   | Genesis Pool C |
-- | Pool #3 | 1M Bcc | Epoch 1_000_000 | Genesis Pool D |
--
-- Pools' metadata are hosted on static local servers started alongside pools.
--
-- # PRE-REGISTERED DATA
--
-- The cluster also comes with a large number of pre-existing faucet wallets and
-- special wallets identified by recovery phrases. Pre-registered wallets can be
-- seen in
--
--   `lib/core-integration/src/Test/Integration/Faucet.hs`.
--
-- All wallets (Cole, Icarus, Sophie) all have 10 UTxOs worth 100_000 Bcc
-- each (so 1M Bcc in total). Additionally, the file also contains a set of
-- wallets with pre-existing rewards (1M Bcc) injected via MIR certificates.
-- These wallets have the same UTxOs as other faucet wallets.
--
-- Some additional wallets of interest:
--
-- - (Sophie) Has a pre-registered stake key but no delegation.
--
--     [ "over", "decorate", "flock", "badge", "beauty"
--     , "stamp", "chest", "owner", "excess", "omit"
--     , "bid", "raccoon", "spin", "reduce", "rival"
--     ]
--
-- - (Sophie) Contains only small coins (but greater than the minUTxOValue)
--
--     [ "either" , "flip" , "maple" , "shift" , "dismiss"
--     , "bridge" , "sweet" , "reveal" , "green" , "tornado"
--     , "need" , "patient" , "wall" , "stamp" , "pass"
--     ]
--
-- - (Sophie) Contains 100 UTxO of 100_000 Bcc, and 100 UTxO of 1 Bcc
--
--     [ "radar", "scare", "sense", "winner", "little"
--     , "jeans", "blue", "spell", "mystery", "sketch"
--     , "omit", "time", "tiger", "leave", "load"
--     ]
--
-- - (Cole) Has only 5 UTxOs of 1,2,3,4,5 Entropic
--
--     [ "suffer", "decorate", "head", "opera"
--     , "yellow", "debate", "visa", "fire"
--     , "salute", "hybrid", "stone", "smart"
--     ]
--
-- - (Cole) Has 200 UTxO, 100 are worth 1 Entropic, 100 are woth 100_000 Bcc.
--
--     [ "collect", "fold", "file", "clown"
--     , "injury", "sun", "brass", "diet"
--     , "exist", "spike", "behave", "clip"
--     ]
--
-- - (Ledger) Created via the Ledger method for master key generation
--
--     [ "struggle", "section", "scissors", "siren"
--     , "garbage", "yellow", "maximum", "finger"
--     , "duty", "require", "mule", "earn"
--     ]
--
-- - (Ledger) Created via the Ledger method for master key generation
--
--     [ "vague" , "wrist" , "poet" , "crazy" , "danger" , "dinner"
--     , "grace" , "home" , "naive" , "unfold" , "april" , "exile"
--     , "relief" , "rifle" , "ranch" , "tone" , "betray" , "wrong"
--     ]
--
-- # CONFIGURATION
--
-- There are several environment variables that can be set to make debugging
-- easier if needed:
--
-- - BCC_WALLET_PORT  (default: random)
--     choose a port for the API to listen on
--
-- - BCC_NODE_TRACING_MIN_SEVERITY  (default: Info)
--     increase or decrease the logging severity of the nodes.
--
-- - BCC_WALLET_TRACING_MIN_SEVERITY  (default: Info)
--     increase or decrease the logging severity of bcc-wallet.
--
-- - TESTS_TRACING_MIN_SEVERITY  (default: Notice)
--     increase or decrease the logging severity of the test cluster framework.
--
-- - LOCAL_CLUSTER_ERA  (default: Jen)
--     By default, the cluster will start in the latest era by enabling
--     "virtual hard forks" in the node config files.
--     The final era can be changed with this variable.
--
-- - TOKEN_METADATA_SERVER  (default: none)
--     Use this URL for the token metadata server.
--
-- - NO_POOLS  (default: stake pools nodes are started and registered)
--     If set, the cluster will only start a BFT leader and a relay, no
--     stake pools. This can be used for running test scenarios which do
--     not require delegation-specific features without paying the
--     startup cost of creating and funding pools.
--
-- - NO_CLEANUP  (default: temp files are cleaned up)
--     If set, the temporary directory used as a state directory for
--     nodes and wallet data won't be cleaned up.
main :: IO ()
main = withLocalClusterSetup $ \dir clusterLogs walletLogs ->
    withLoggingNamed "cluster" clusterLogs $ \(_, (_, trCluster)) -> do
        let tr' = contramap MsgCluster $ trMessageText trCluster
        clusterCfg <- localClusterConfigFromEnv
        withCluster tr' dir clusterCfg
            (setupFaucet dir (trMessageText trCluster))
            (whenReady dir (trMessageText trCluster) walletLogs)
  where
    setupFaucet dir trCluster (RunningNode socketPath _ _) = do
        traceWith trCluster MsgSettingUpFaucet
        let trCluster' = contramap MsgCluster trCluster
        let encodeAddresses = map (first (T.unpack . encodeAddress @'Mainnet))
        let accts = concatMap genRewardAccounts mirMnemonics
        let rewards = (, Coin $ fromIntegral oneMillionBcc) <$> accts

        sendFaucetFundsTo trCluster' socketPath dir $
            encodeAddresses sophieIntegrationTestFunds
        sendFaucetAssetsTo trCluster' socketPath dir 20 $ encodeAddresses $
            jenIntegrationTestAssets (Coin 1_000_000_000)
        moveInstantaneousRewardsTo trCluster' socketPath dir rewards

    whenReady dir trCluster logs (RunningNode socketPath block0 (gp, vData)) =
        withLoggingNamed "bcc-wallet" logs $ \(sb, (cfg, tr)) -> do

            ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

            let tracers = setupTracers (tracerSeverities (Just Debug)) tr
            let db = dir </> "wallets"
            createDirectory db
            listen <- walletListenFromEnv
            tokenMetadataServer <- tokenMetadataServerFromEnv

            prometheusUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)
                )
                <$> getPrometheusURL
            ekgUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)
                )
                <$> getEKGURL

            void $ serveWallet
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                tracers
                (SyncTolerance 10)
                (Just db)
                Nothing
                "127.0.0.1"
                listen
                Nothing
                Nothing
                tokenMetadataServer
                socketPath
                block0
                (gp, vData)
                (\u -> traceWith trCluster $ MsgBaseUrl (T.pack . show $ u)
                    ekgUrl prometheusUrl)

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup
    :: (FilePath -> [LogOutput] -> [LogOutput] -> IO a)
    -> IO a
withLocalClusterSetup action = do
    -- Handle SIGTERM properly
    installSignalHandlers (putStrLn "Terminated")

    -- Ensure key files have correct permissions for bcc-cli
    setDefaultFilePermissions

    -- Set UTF-8, regardless of user locale
    withUtf8Encoding $
        -- This temporary directory will contain logs, and all other data
        -- produced by the local test cluster.
        withSystemTempDir stdoutTextTracer "test-cluster" $ \dir -> do
            let logOutputs name minSev =
                    [ LogToFile (dir </> name) (min minSev Info)
                    , LogToStdStreams minSev ]

            clusterLogs <- logOutputs "cluster.log" <$> testMinSeverityFromEnv
            walletLogs <- logOutputs "wallet.log" <$> walletMinSeverityFromEnv

            action dir clusterLogs walletLogs

-- Logging

data TestsLog
    = MsgBaseUrl Text Text Text -- wallet url, ekg url, prometheus url
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBaseUrl walletUrl ekgUrl prometheusUrl -> mconcat
            [ "Wallet url: " , walletUrl
            , ", EKG url: " , ekgUrl
            , ", Prometheus url:", prometheusUrl
            ]
        MsgSettingUpFaucet -> "Setting up faucet..."
        MsgCluster msg -> toText msg

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl {} -> Notice
        MsgCluster msg -> getSeverityAnnotation msg
