module Bitcoin where

import qualified Data.ByteString as B

newtype State = State [(Int, B.ByteString)]

