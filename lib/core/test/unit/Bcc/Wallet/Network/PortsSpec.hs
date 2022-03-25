module Bcc.Wallet.Network.PortsSpec
    ( spec
    ) where

import Prelude

import Bcc.Wallet.Network.Ports
    ( getRandomPort, isPortOpen, simpleSockAddr )
import Network.HTTP.Types
    ( status200 )
import Network.Wai
    ( responseLBS )
import Network.Wai.Handler.Warp
    ( withApplication )
import Test.Hspec
    ( Spec, describe, it, shouldReturn )

spec :: Spec
spec = describe "Bcc.Wallet.Network.Ports" $ do
    it "isPortOpen detects an available port" $ do
        port <- getRandomPort
        isPortOpen (localhost port) `shouldReturn` False

    it "isPortOpen detects a port in use" $ do
        let app _req respond = respond $ responseLBS status200 [] ""
        withApplication (pure app) $ \port ->
            isPortOpen (localhost (fromIntegral port)) `shouldReturn` True

  where
    localhost = simpleSockAddr (127,0,0,1)

