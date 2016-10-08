module Data.Binary.Strict where

import qualified Data.Binary as Lazy
import           Data.Binary (Binary)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy


encode :: Binary a => a -> Strict.ByteString
encode = Lazy.toStrict . Lazy.encode

decode :: Binary a => Strict.ByteString -> a
decode = Lazy.decode . Lazy.fromStrict
