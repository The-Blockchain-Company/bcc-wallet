{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bcc.Wallet.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Bcc.Wallet.DB.Properties
    ( properties )
import Bcc.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import Bcc.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Bcc.Wallet.Primitive.AddressDerivation.Sophie
    ( SophieKey )
import Bcc.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Bcc.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
import Bcc.Wallet.Primitive.Types.Address
    ( Address )
import Control.DeepSeq
    ( NFData )
import Test.Hspec
    ( Spec, before, describe )
import Test.QuickCheck
    ( Arbitrary (..) )
import Test.Utils.Platform
    ( pendingOnMacOS )

import qualified Bcc.Wallet.DB.MVar as MVar

spec :: Spec
spec =
    before (pendingOnMacOS "#2472: timeouts in hydra mac builds")
    $ before (MVar.newDBLayer @IO @(SeqState 'Mainnet SophieKey) ti)
    $ describe "MVar" properties
  where
    ti = dummyTimeInterpreter

newtype DummyStateMVar = DummyStateMVar Int
    deriving (Show, Eq)

instance Arbitrary DummyStateMVar where
    shrink _ = []
    arbitrary = DummyStateMVar <$> arbitrary

deriving instance NFData DummyStateMVar

instance IsOurs DummyStateMVar Address where
    isOurs _ num = (Nothing, num)
