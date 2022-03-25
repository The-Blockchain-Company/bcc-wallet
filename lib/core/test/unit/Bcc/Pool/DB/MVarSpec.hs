-- |
-- Copyright: © 2021-2022 TBCO
-- License: Apache-2.0
--
-- Tests for the 'MVar' implementation of the pool 'DBLayer' interface.
--
module Bcc.Pool.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Bcc.Pool.DB.Properties
    ( properties )
import Bcc.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import Test.Hspec
    ( Spec, before, describe )

import qualified Bcc.Pool.DB.MVar as MVar

spec :: Spec
spec = before (MVar.newDBLayer ti) $
    describe "MVar" properties
  where
    ti = dummyTimeInterpreter
