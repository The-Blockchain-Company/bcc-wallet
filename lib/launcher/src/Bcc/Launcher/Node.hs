{-# LANGUAGE TupleSections #-}
-- |
-- Copyright: © 2021-2022 TBCO
-- License: Apache-2.0
--
-- Provides a function to launch @bcc-node@.

module Bcc.Launcher.Node
    ( -- * Startup
      withBccNode
    , BccNodeConfig (..)
    , NodePort (..)

      -- * bcc-node Snockets
    , BccNodeConn
    , bccNodeConn
    , nodeSocketFile
    , isWindows
    ) where

import Prelude

import Bcc.Launcher
    ( LauncherLog, ProcessHasExited, withBackendCreateProcess )
import Control.Tracer
    ( Tracer (..) )
import Data.Bifunctor
    ( first )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( maybeToList )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import System.Environment
    ( getEnvironment )
import System.FilePath
    ( isValid, takeFileName, (</>) )
import System.Info
    ( os )
import UnliftIO.Process
    ( CreateProcess (..), proc )

import qualified Data.Text as T

-- | Parameters for connecting to the node.
newtype BccNodeConn = BccNodeConn FilePath
    deriving (Show, Eq)

-- | Gets the socket filename or pipe name from 'BccNodeConn'. Whether it's
-- a unix socket or named pipe depends on the value of 'isWindows'.
nodeSocketFile :: BccNodeConn -> FilePath
nodeSocketFile (BccNodeConn name) = name

-- | Produces a 'BccNodeConn' if the socket path or pipe name (depending on
-- 'isWindows') is valid.
bccNodeConn :: FilePath -> Either String BccNodeConn
bccNodeConn name
    | isWindows = if isValidWindowsPipeName name
        then Right $ BccNodeConn name
        else Left "Invalid pipe name."
    | otherwise = if isValid name
        then Right $ BccNodeConn name
        else Left "Invalid file path."

isWindows :: Bool
isWindows = os == "mingw32"

isValidWindowsPipeName :: FilePath -> Bool
isValidWindowsPipeName name = slashPipe `isPrefixOf` name
    && isValid (drop (length slashPipe) name)
  where
    slashPipe = "\\\\.\\pipe\\"

instance ToText BccNodeConn where
    toText = T.pack . nodeSocketFile

instance FromText BccNodeConn where
    fromText = first TextDecodingError . bccNodeConn . T.unpack

newtype NodePort = NodePort { unNodePort :: Int }
    deriving (Show, Eq)

-- | A subset of the @bcc-node@ CLI parameters, used for starting the
-- backend.
data BccNodeConfig = BccNodeConfig
    { nodeDir             :: FilePath
    , nodeConfigFile      :: FilePath
    , nodeTopologyFile    :: FilePath
    , nodeDatabaseDir     :: FilePath
    , nodeDlgCertFile     :: Maybe FilePath
    , nodeSignKeyFile     :: Maybe FilePath
    , nodeOpCertFile      :: Maybe FilePath
    , nodeKesKeyFile      :: Maybe FilePath
    , nodeVrfKeyFile      :: Maybe FilePath
    , nodePort            :: Maybe NodePort
    , nodeLoggingHostname :: Maybe String
    } deriving (Show, Eq)

-- | Spawns a @bcc-node@ process.
--
-- IMPORTANT: @bcc-node@ must be available on the current path.
withBccNode
    :: Tracer IO LauncherLog
    -- ^ Trace for subprocess control logging
    -> BccNodeConfig
    -> (BccNodeConn -> IO a)
    -- ^ Callback function with a socket filename and genesis params
    -> IO (Either ProcessHasExited a)
withBccNode tr cfg action = do
    let socketPath = nodeSocketPath (nodeDir cfg)
    cp <- bccNodeProcess cfg socketPath
    withBackendCreateProcess tr cp $ \_ _ -> action $ BccNodeConn socketPath

{-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------}

-- | Generate command-line arguments for launching @bcc-node@.
bccNodeProcess :: BccNodeConfig -> FilePath -> IO CreateProcess
bccNodeProcess cfg socketPath = do
    myEnv <- getEnvironment
    let env' = ("BCC_NODE_LOGGING_HOSTNAME",) <$> nodeLoggingHostname cfg

    pure $ (proc "bcc-node" args)
        { env = Just $ maybeToList env' ++ myEnv
        , cwd = Just $ nodeDir cfg
        }
  where
    args =
        [ "run"
        , "--config", nodeConfigFile cfg
        , "--topology", nodeTopologyFile cfg
        , "--database-path", nodeDatabaseDir cfg
        , "--socket-path", socketPath
        ]
        ++ opt "--port" (show . unNodePort <$> nodePort cfg)
        ++ opt "--signing-key" (nodeSignKeyFile cfg)
        ++ opt "--delegation-certificate" (nodeDlgCertFile cfg)
        ++ opt "--sophie-operational-certificate" (nodeOpCertFile cfg)
        ++ opt "--sophie-kes-key" (nodeKesKeyFile cfg)
        ++ opt "--sophie-vrf-key" (nodeVrfKeyFile cfg)
        ++ ["+RTS", "-N4", "-RTS"]

    opt _ Nothing = []
    opt arg (Just val) = [arg, val]

-- | Generate a 'FilePath' for the @bcc-node@ domain socket/named pipe.
nodeSocketPath
    :: FilePath -- ^ @bcc-node@ state directory
    -> FilePath -- ^ UNIX socket file path or Windows named pipe name
nodeSocketPath dir
    | os == "mingw32" = "\\\\.\\pipe\\" ++ takeFileName dir
    | otherwise = dir </> "node.socket"
