{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2021-2022 TBCO
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Shared' Keys.

module Bcc.Wallet.Primitive.AddressDerivation.Shared
    ( -- * Types
      SharedKey(..)

    -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed

    , purposeCIP1854
    ) where

import Prelude

import Bcc.Address.Derivation
    ( xpubPublicKey )
import Bcc.Crypto.Wallet
    ( XPrv, XPub, toXPub, unXPrv, unXPub, xPrvChangePass, xprv, xpub )
import Bcc.Mnemonic
    ( SomeMnemonic )
import Bcc.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , fromHex
    , hex
    )
import Bcc.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..), purposeCIP1854 )
import Bcc.Wallet.Primitive.AddressDerivation.Sophie
    ( deriveAccountPrivateKeySophie
    , deriveAddressPrivateKeySophie
    , deriveAddressPublicKeySophie
    , unsafeGenerateKeyFromSeedSophie
    )
import Bcc.Wallet.Primitive.AddressDiscovery
    ( GetPurpose (..) )
import Bcc.Wallet.Primitive.Types.Address
    ( Address (..) )
import Bcc.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.Monad
    ( (<=<) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )

import qualified Data.ByteString as BS

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
       -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> SharedKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> SharedKey depth XPrv
unsafeGenerateKeyFromSeed mnemonics pwd =
    SharedKey $ unsafeGenerateKeyFromSeedSophie mnemonics pwd

instance HardDerivation SharedKey where
    type AddressIndexDerivationType SharedKey = 'Soft

    deriveAccountPrivateKey pwd (SharedKey rootXPrv) ix =
        SharedKey $ deriveAccountPrivateKeySophie purposeCIP1854 pwd rootXPrv ix

    deriveAddressPrivateKey pwd (SharedKey accXPrv) role ix =
        SharedKey $ deriveAddressPrivateKeySophie pwd accXPrv role ix

instance SoftDerivation SharedKey where
    deriveAddressPublicKey (SharedKey accXPub) role ix =
        SharedKey $ deriveAddressPublicKeySophie accXPub role ix

{-------------------------------------------------------------------------------
                            WalletKey implementation
-------------------------------------------------------------------------------}

instance WalletKey SharedKey where
    changePassphrase (Passphrase oldPwd) (Passphrase newPwd) (SharedKey prv) =
        SharedKey $ xPrvChangePass oldPwd newPwd prv

    publicKey (SharedKey prv) =
        SharedKey (toXPub prv)

    digest (SharedKey pub) =
        hash (unXPub pub)

    getRawKey =
        getKey

    liftRawKey =
        SharedKey

    keyTypeDescriptor _ =
        "sha"

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance GetPurpose SharedKey where
    getPurpose = purposeCIP1854

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (SharedKey 'RootK) where
    serializeXPrv (k, h) =
        ( hex . unXPrv . getKey $ k
        , hex . getHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap SharedKey (xprvFromText k)
        <*> fmap Hash (fromHex h)
      where
        xprvFromText = xprv <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPrv: unable to deserialize SharedKey"

instance PersistPublicKey (SharedKey depth) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err SharedKey . (xpub <=< fromHex @ByteString)
      where
        err _ = error "unsafeDeserializeXPub: unable to deserialize SharedKey"

instance MkKeyFingerprint SharedKey Address where
    paymentKeyFingerprint (Address bytes) =
        Right $ KeyFingerprint $ BS.take hashSize $ BS.drop 1 bytes

instance MkKeyFingerprint SharedKey (Proxy (n :: NetworkDiscriminant), SharedKey 'AddressK XPub) where
    paymentKeyFingerprint (_, paymentK) =
        Right $ KeyFingerprint $ blake2b224 $ xpubPublicKey $ getKey paymentK

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

hashSize :: Int
hashSize =
    hashDigestSize Blake2b_224
