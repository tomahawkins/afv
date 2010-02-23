module Error
  ( err
  , notSupported
  , unexpected
  , position
  , debug
  , debug'
  ) where

import Language.C
import System.IO.Unsafe

err :: Pos a => a -> String -> b
err a m = error $ position a ++ ": " ++ m

notSupported :: Pos a => a -> String -> b
notSupported a m = err a $ "not supported: " ++ m

unexpected :: Pos a => a -> String -> b
unexpected a m = err a $ "unexpected: " ++ m

debug :: Show a => String -> a -> a
debug m a = debug' m a a

debug' :: Show a => String -> a -> b -> b
debug' m a b = unsafePerformIO (putStrLn (m ++ ": " ++ show a) >> return b)

position :: Pos a => a -> String
position a = f ++ ":" ++ show l ++ ":" ++ show c
  where
  Position f l c = posOf a
