module Data.Binary.Strict
  ( Lazy.Binary  -- re-export Binary so users don't need to also import the
                 -- original Data.Binary
  , module Data.Binary.Strict  -- also export the rest of the module as usual
  ) where

import qualified Data.Binary as Lazy
import           Data.Binary (Binary)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Debug.Trace



encode :: Binary a => a -> Strict.ByteString
encode x = let strictString = Lazy.toStrict $ Lazy.encode x
            in trace (show strictString) strictString

decode :: Binary a => Strict.ByteString -> a
decode strictString = let lazyString = Lazy.fromStrict strictString
                       in case Lazy.decodeOrFail lazyString of
                            Left err -> error (show (strictString, err))
                            Right (_,_,x) -> x
