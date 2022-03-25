module Bcc.Wallet.TransactionSpec
    ( spec
    ) where

import Prelude

import Bcc.Wallet.Primitive.Types.Address
    ( Address (..) )
import Bcc.Wallet.Transaction
    ( ErrMkTransaction (..) )
import Test.Hspec
    ( Spec, describe, it )

spec :: Spec
spec =
    describe "Pointless tests to cover 'Show' instances for errors" $ do
        testShow $ ErrKeyNotFoundForAddress $ Address mempty

testShow :: Show a => a -> Spec
testShow a = it (show a) True
