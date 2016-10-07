{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.MyExtra where

import Data.Function
import Data.Proxy
import Data.Typeable
import Text.Printf


fromRightM :: (Show e, Monad m) => Either e a -> m a
fromRightM (Left  e) = fail $ show e
fromRightM (Right x) = return x

readM :: forall m a. (Monad m, Read a, Typeable a) => String -> m a
readM s = reads s & \case
    [(x,"")] -> return x
    _        -> fail $ printf "expected %s, found %s"
                              (show $ typeRep (Proxy :: Proxy a))
                              (show s)

untilM :: Monad m => m (Maybe a) -> m a
untilM body = body >>= \case
    Nothing -> untilM body
    Just x  -> return x
