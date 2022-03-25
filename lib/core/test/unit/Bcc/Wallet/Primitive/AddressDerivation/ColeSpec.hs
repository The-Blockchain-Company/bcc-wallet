{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bcc.Wallet.Primitive.AddressDerivation.ColeSpec
    ( spec
    ) where

import Prelude

import Bcc.Address.Derivation
    ( XPrv )
import Bcc.Mnemonic
    ( SomeMnemonic (..) )
import Bcc.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..), Passphrase (..) )
import Bcc.Wallet.Primitive.AddressDerivation.Cole
    ( ColeKey (..)
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    , unsafeMkColeKeyFromMasterKey
    )
import Bcc.Wallet.Primitive.AddressDerivationSpec
    ()
import Bcc.Wallet.Unsafe
    ( unsafeMkMnemonic, unsafeXPrv )
import Control.Monad
    ( (<=<) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, vector )

import qualified Bcc.Crypto.Wallet as CC
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    goldenSpec
    parallel $ describe "Random Address Derivation Properties" $ do
        it "Key derivation from seed works for various indexes" $
            property prop_keyDerivationFromSeed
        it "Key derivation from master key works for various indexes" $
            property prop_keyDerivationFromXPrv

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivationFromSeed
    :: SomeMnemonic
    -> Passphrase "encryption"
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_keyDerivationFromSeed seed encPwd accIx addrIx =
    rndKey `seq` property () -- NOTE Making sure this doesn't throw
  where
    rndKey :: ColeKey 'AddressK XPrv
    rndKey = unsafeGenerateKeyFromSeed (accIx, addrIx) seed encPwd

prop_keyDerivationFromXPrv
    :: XPrv
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> Property
prop_keyDerivationFromXPrv masterkey accIx addrIx =
    rndKey `seq` property () -- NOTE Making sure this doesn't throw
  where
    rndKey :: ColeKey 'AddressK XPrv
    rndKey = unsafeMkColeKeyFromMasterKey (accIx, addrIx) masterkey

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpec :: Spec
goldenSpec = parallel $ describe "Golden tests" $ do
    it "generateKeyFromSeed - no passphrase" $
        generateTest generateTest1

    it "generateKeyFromSeed - with passphrase" $
        generateTest generateTest2

{-------------------------------------------------------------------------------
                      Golden tests for generateKeyFromSeed
-------------------------------------------------------------------------------}

data GenerateKeyFromSeed = GenerateKeyFromSeed
    { mnem :: [Text]
    , pwd :: Passphrase "encryption"
    , rootKey :: ColeKey 'RootK XPrv
    }

generateTest :: GenerateKeyFromSeed -> Expectation
generateTest GenerateKeyFromSeed{..} =
    getKey (generateKeyFromSeed mw pwd)
    `shouldBe`
    getKey rootKey
  where
    mw = SomeMnemonic $ unsafeMkMnemonic @12 mnem

generateTest1 :: GenerateKeyFromSeed
generateTest1 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp ""
    , rootKey = xprv16
        "b84d0b6db447911a98a3ade98145c0e8323e106f07bc17a99c2104c2688bb752831090\
        \2a3cec7e262ded6a4369ec1f48966a6b48b1ee90aa00e61b95417949f81258854ab44b\
        \0cfda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda\
        \5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

generateTest2 :: GenerateKeyFromSeed
generateTest2 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp "4a87f05fe25a57c96ff5221863e61b91bcca566b853b616f55e5d2c18caa1a4c"
    , rootKey = xprv16
        "b842ae13cbb31b7d96910472bbed5c8729c764d66af81b48120a6a583eae55faf78c247\
        \65e0c9826f4d095f3e6addb4bda68df322b220d3c08b8a5b414232d101258854ab44b0c\
        \fda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda5b2\
        \57eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

-- | This is the mnemonic that provides the 'Default' instance in bcc-sl
defMnemonic :: [Text]
defMnemonic =
    [ "squirrel", "material", "silly", "twice", "direct", "slush"
    , "pistol", "razor", "become", "junk", "kingdom", "flee" ]

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Get a private key from a hex string, without error checking.
xprv16 :: ByteString -> ColeKey 'RootK XPrv
xprv16 hex = ColeKey k () (error "passphrase not used for tests")
  where
    Right k = xprvFromText hex
    xprvFromText = CC.xprv <=< fromHexText
    fromHexText :: ByteString -> Either String ByteString
    fromHexText = convertFromBase Base16

-- | Get a passphrase from a hex string, without error checking
pp :: ByteString -> Passphrase depth
pp hex = Passphrase b
    where Right b = convertFromBase Base16 hex

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

-- This generator will only produce valid (@>= minSeedLengthBytes@) passphrases
-- because 'generateKeyFromSeed' is a partial function.
instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vector n
        return $ Passphrase $ BA.convert bytes

instance Arbitrary XPrv where
    arbitrary = unsafeXPrv . BS.pack <$> vector 128
