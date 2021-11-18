module ParserUtil where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Base16.Lazy as Base16
import Data.Binary.Get
import Data.Binary.Put
import GHC.Word
import Control.Applicative ((<|>))
import GHC.Int (Int64)

-- | Parse Bitcoin's VarInt.
getVI :: Integral a => Get a
getVI = 
  cast (be 0xFF >> getWord64le) <|>
  cast (be 0xFE >> getWord32le) <|>
  cast (be 0xFD >> getWord16le) <|>
  cast getWord8

-- | Serialize Bitcoin's VarInt.
putVI :: Integral a => a -> Put
putVI n
  | n <= 0xfc = putWord8 (fromIntegral n)
  | n <= 0xffff = putWord8 0xFD >> putWord16le (fromIntegral n)
  | n <= 0xffffffff = putWord8 0xFE >> putWord32le (fromIntegral n)
  | otherwise = putWord8 0xFF >> putWord64le (fromIntegral n)

-- | Match a specific byte.
be :: Word8 -> Get Word8
be x = do
  w <- getWord8
  if w == x
    then return w
    else fail "mismatch at be"

-- | Cast to an Integral to another.
cast :: (Integral a, Integral b) => Get a -> Get b
cast = fmap fromIntegral

-- | if given bool is true, do given action and return Just. if false, return Nothing.
opt :: Monad m => Bool -> m a -> m (Maybe a)
opt b a = if b then Just <$> a else return Nothing

-- | hex string to byte string
decodeHex :: String -> B.ByteString
decodeHex = fst . Base16.decode . Char8.pack

encodeHex :: B.ByteString -> String
encodeHex = Char8.unpack . Base16.encode