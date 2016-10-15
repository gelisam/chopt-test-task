{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.MyExtra where

import Data.Function
import Data.Proxy
import Data.Typeable
import Text.Printf


-- run the body until its accumulated results reaches the limit
untilTotalM :: Monad m => Int -> m Int -> m ()
untilTotalM limit body = loop 0
  where
    loop !acc | acc >= limit = return ()
              | otherwise    = do
      n <- body
      loop (acc + n)

-- convert 'Either' failures into other kinds of monadic failures
fromRightM :: (Show e, Monad m) => Either e a -> m a
fromRightM (Left  e) = fail $ show e
fromRightM (Right x) = return x

-- a version of 'read' which fails with a more readable error message
readM :: forall m a. (Monad m, Read a, Typeable a) => String -> m a
readM s = reads s & \case
    [(x,"")] -> return x
    _        -> fail $ printf "expected %s, found %s"
                              (show $ typeRep (Proxy :: Proxy a))
                              (show s)

-- run the body until it return 'Just'
untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM body = body >>= \case
    Nothing -> untilJustM body
    Just x  -> return x
