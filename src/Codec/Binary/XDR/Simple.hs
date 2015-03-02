{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances  #-}

-- | This script implements a subset of XDR -- eXternal Data Representation (RFC: 4506)
--
-- A thin wrapper around @Data.Binary.Get@\/@Put@ from <http://hackage.haskell.org/package/binary binary> package, inspired by Python's <http://docs.python.org/2/library/xdrlib.html xdrlib>.

module Codec.Binary.XDR.Simple where

import           Control.Monad                  (mapM_, replicateM, replicateM_, when)
import           Data.Binary                    (get, put, putWord8)
import           Data.Binary.IEEE754            (getFloat32be, getFloat64be, putFloat32be, putFloat64be)
import           Data.Binary.Get                (Get, getLazyByteString, runGet)
import           Data.Binary.Put                (Put, putLazyByteString, runPut)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Int                       (Int32, Int64)
import           Data.Word                      (Word32, Word64)
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T
import           Foreign.Marshal.Alloc          (alloca)
import           Foreign.Ptr                    (castPtr)
import           Foreign.Storable               (Storable, peek, poke)
import           System.IO.Unsafe               (unsafePerformIO)

type Pack = Put

-- | Pack all 'pack' sequence, run Put action(s)
xdrPack :: Pack -> LBS.ByteString
xdrPack = runPut

class XDRpack a where
    -- | Pack a value of type @a@
    --
    -- /Note/:
    --
    -- * By default, Int32, Int64, Word32, Word64 are 'put' in big-endian (network) order
    -- * 'pack'ing of quadruple-precision floating-point numbers is not implemented
    pack :: a -> Pack

-- | Int(@Int32@) is a 32-bit signed integer, in the range [-2147483648,2147483647]
instance XDRpack Int32 where
    pack = put

-- | Hyper is a 64-bit(8-byte) signed integer
type Hyper = Int64

-- | Hyper is a 64-bit(8-byte) signed integer
instance XDRpack Hyper where
    pack = put

-- | UInt is a 32-bit unsigned integer, in the range [0,4294967295]
type UInt = Word32

-- | UInt is a 32-bit unsigned integer, in the range [0,4294967295]
instance XDRpack UInt where
    pack = put

-- | UHyper is a 64-bit(8-byte) unsigned integer
type UHyper = Word64

-- | UHyper is a 64-bit(8-byte) unsigned integer
instance XDRpack UHyper where
    pack = put

-- | Float is a 32-bit (4-byte) single-precision floating-point number
instance XDRpack Float where
    pack = putFloat32be

-- | Double is a 64-bit (8-byte) double-precision floating-point number
instance XDRpack Double where
    pack = putFloat64be

-- | A 32-bit unsigned integer, @False@ = 0, @True@ = 1
instance XDRpack Bool where
    pack True  = put (1 :: Word32)
    pack False = put (0 :: Word32)

-- | For opaque data & 8bit-safe strings
instance XDRpack LBS.ByteString where
    pack w8s
        | len <= 2^32-1 = do
            pack (fromIntegral len :: UInt)
            putLazyByteString w8s
            let align = fromIntegral (len `mod` 4) :: Int
            when (align /= 0) $
                replicateM_ (4-align) (putWord8 0) -- padding
        | otherwise = error "cannot pack bytestring longer than 2**32-1"
        where len = LBS.length w8s :: Int64

-- | For Haskell strings, UTF-8 encoding assumed. You always use UTF-8, /right?/
instance XDRpack String where
    pack = pack . utf8ToWord8s
        where utf8ToWord8s :: String -> LBS.ByteString
              utf8ToWord8s = T.encodeUtf8 . T.pack

-- | Array (list)
instance XDRpack a => XDRpack [a] where
    pack array
        | len <= 2^32-1 = do
            pack (fromIntegral len :: UInt)
            mapM_ pack array
        | otherwise = error "cannot pack list/array longer than 2**32-1"
        where len = length array

type Unpack = Get

-- | Unpack all 'unpack' sequence, run @Get a@ action(s)
xdrUnpack :: Unpack a -> LBS.ByteString -> a
xdrUnpack = runGet

class XDRunpack a where
    -- | Unpack a value of type @a@
    unpack :: Unpack a

instance XDRunpack Int32 where
    unpack = get

instance XDRunpack Hyper where
    unpack = get

instance XDRunpack UInt where
    unpack = get

instance XDRunpack UHyper where
    unpack = get

instance XDRunpack Bool where
    unpack = w2b `fmap` unpack
        where w2b :: Word32 -> Bool
              w2b 0 = False
              w2b _ = True

instance XDRunpack Float where
    unpack = getFloat32be

instance XDRunpack Double where
    unpack = getFloat64be

instance XDRunpack LBS.ByteString where
    unpack = do
        n :: UInt <- unpack
        let margin = ((n + 3) `div` 4) * 4
        padded <- getLazyByteString (fromIntegral margin)
        let s = LBS.takeWhile (/= 0) padded
        return s

instance XDRunpack String where
    unpack = word8sToUtf8 `fmap` unpack
        where word8sToUtf8 :: LBS.ByteString -> String
              word8sToUtf8 = T.unpack . T.decodeUtf8

instance XDRunpack a => XDRunpack [a] where
    unpack = do
        n :: UInt <- unpack
        replicateM (fromIntegral n) (unpack :: Unpack a)