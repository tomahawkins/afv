module Error
  ( err
  , notSupported
  , unexpected
  , err'
  , notSupported'
  , unexpected'
  , position
  ) where

import Language.C

err :: (Pretty a, Pos a) => a -> String -> b
err a m = error $ position a ++ ": " ++ m ++ ": " ++ show (pretty a)

notSupported :: (Pretty a, Pos a) => a -> String -> b
notSupported a m = err a $ "not supported: " ++ m

unexpected :: (Pretty a, Pos a) => a -> String -> b
unexpected a m = err a $ "unexpected: " ++ m

err' :: Pos a => a -> String -> b
err' a m = error $ position a ++ ": " ++ m

notSupported' :: Pos a => a -> String -> b
notSupported' a m = err' a $ "not supported: " ++ m

unexpected' :: Pos a => a -> String -> b
unexpected' a m = err' a $ "unexpected: " ++ m

position :: Pos a => a -> String
position a = f ++ ":" ++ show l ++ ":" ++ show c
  where
  Position f l c = posOf a

