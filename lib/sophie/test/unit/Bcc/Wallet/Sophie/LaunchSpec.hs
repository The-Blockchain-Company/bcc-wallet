-- |
-- Copyright: © 2021 TBCO
-- License: Apache-2.0
--

module Bcc.Wallet.Sophie.LaunchSpec (spec) where

import Prelude

import Bcc.Wallet.Sophie.Launch
    ( nodeSocketOption )
import Options.Applicative
    ( ParserResult (..), defaultPrefs, execParserPure, info )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy )
import Test.Utils.Platform
    ( isWindows )

spec :: Spec
spec = describe "Sophie CLI" $
    describe "Node socket/pipe option" $ do
        let parse arg = execParserPure defaultPrefs
                (info nodeSocketOption mempty)
                ["--node-socket", arg]
        let ok (Success _) = True
            ok _ = False
        let err (Failure _) = True
            err _ = False
        let runTest (desc, arg, posix, windows) = it desc $ do
                let tst = if isWindows then windows else posix
                parse arg `shouldSatisfy` tst
        mapM_ runTest
            [ ( "valid rel path",   "hello.sock",     ok,  err )
            , ( "valid abs path",   "/run/cw.sock",   ok,  err )
            , ( "invalid",          "",               err, err )
            , ( "valid pipename",   "\\\\.\\pipe\\a", ok,  ok  )
            , ( "invalid pipename", "\\\\.\\pipe\\",  ok,  err )
            , ( "windows path",     "c:\\windows",    ok,  err )
            ]
