{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Transport.TCP.Address where

import qualified Data.ByteString.Char8 as ByteString
import           Data.Function
import           Data.List.Split
import           Network.Transport
import           Text.Printf

import           Control.Monad.MyExtra
import           Text.Parsable


-- Addresses from Network.Transport.TCP look like "localhost:8080:0"
data Address = Address
  { addressHost    :: !String
  , addressPort    :: !Int
  , addressChannel :: !Int
  }

instance Parsable Address where
    parse s = s & splitOn ":" & \case
        [host, port, channel] -> Address
                             <$> pure host
                             <*> readM port
                             <*> readM channel
        _ -> fail $ printf "expected an address of the style %s, got %s"
                           (show "localhost:8080:0")
                           (show s)
    unparse (Address {..}) = printf "%s:%d:%d" addressHost addressPort addressChannel


endpointAddress :: Address -> EndPointAddress
endpointAddress = EndPointAddress
                . ByteString.pack
                . unparse
