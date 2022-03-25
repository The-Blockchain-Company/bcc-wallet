{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2021-2022 TBCO
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.

module Bcc.Pool.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Bcc.BM.Trace
    ( nullTracer )
import Bcc.DB.Sqlite
    ( DBLog (..) )
import Bcc.Pool.DB
    ( DBLayer (..) )
import Bcc.Pool.DB.Log
    ( PoolDbLog (..) )
import Bcc.Pool.DB.Properties
    ( properties )
import Bcc.Pool.DB.Sqlite
    ( withDBLayer )
import Bcc.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import System.Directory
    ( copyFile )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, around, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Trace
    ( captureLogging )
import UnliftIO.Temporary
    ( withSystemTempDirectory )

withMemoryDBLayer
    :: (DBLayer IO -> IO a)
    -> IO a
withMemoryDBLayer = withDBLayer nullTracer Nothing dummyTimeInterpreter

spec :: Spec
spec = parallel $ do
    around withMemoryDBLayer $ do
        parallel $ describe "Sqlite" properties

    describe "Migration Regressions" $ do
        test_migrationFromv20191216

test_migrationFromv20191216 :: Spec
test_migrationFromv20191216 =
    it "'migrate' an existing database from v2019-12-16 by\
       \ creating it from scratch again. But only once." $ do
        let orig = $(getTestData) </> "stake-pools-db" </> "v2019-12-16.sqlite"
        withSystemTempDirectory "stake-pools-db" $ \dir -> do
            let path = dir </> "stake-pools.sqlite"
            copyFile orig path
            let ti = dummyTimeInterpreter
            (logs, _) <- captureLogging $ \tr -> do
                withDBLayer tr (Just path) ti $ \_ -> pure ()
                withDBLayer tr (Just path) ti $ \_ -> pure ()

            let databaseConnMsg  = filter isMsgOpenDB logs
            let databaseResetMsg = filter (== MsgGeneric MsgDatabaseReset) logs
            let migrationErrMsg  = filter isMsgMigrationError logs

            length databaseConnMsg  `shouldBe` 3
            length databaseResetMsg `shouldBe` 1
            length migrationErrMsg  `shouldBe` 1

isMsgOpenDB :: PoolDbLog -> Bool
isMsgOpenDB (MsgGeneric (MsgStartConnectionPool _)) = True
isMsgOpenDB _ = False

isMsgMigrationError :: PoolDbLog -> Bool
isMsgMigrationError (MsgGeneric (MsgMigrations (Left _))) = True
isMsgMigrationError _ = False
