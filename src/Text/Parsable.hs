{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.Parsable where

import           Text.Printf


-- a variant of Read+Show which doesn't require the string form to be valid Haskell code.
-- 
-- laws:
--   parse . unparse = return
--   parse s >> parse s = parse s
class Parsable a where
    parse   :: Monad m => String -> m a
    unparse :: a -> String


-- represent () as ""
instance Parsable () where
    parse "" = return ()
    parse s  = fail $ printf "expected the empty string but got %s" (show s)
    unparse () = ""

-- represent strings as themselves
instance Parsable String where
    parse = return
    unparse = id
