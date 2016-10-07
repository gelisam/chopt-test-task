{-# LANGUAGE LambdaCase #-}
module Control.Monad.MyExtra where


fromRightM :: (Show e, Monad m) => Either e a -> m a
fromRightM (Left  e) = fail $ show e
fromRightM (Right x) = return x

untilM :: Monad m => m (Maybe a) -> m a
untilM body = body >>= \case
    Nothing -> untilM body
    Just x  -> return x
