module Parse (parse) where

import Language.C
import Language.C.System.GCC
import Language.C.System.Preprocess
import System.Exit

parse :: FilePath -> [FilePath] -> [(String,String)] -> FilePath -> IO CTranslUnit
parse gcc includes defines file = do
  a <- preprocess gcc includes defines file
  case parseC a (initPos file) of
    Left e  -> putStrLn ("parsing error: " ++ show e) >> exitFailure
    Right a -> return a

preprocess :: FilePath -> [FilePath] -> [(String,String)] -> FilePath -> IO InputStream
preprocess gcc includes defines file = do
  a <- runPreprocessor (newGCC gcc) CppArgs
    { cppOptions = map IncludeDir includes ++ map (\ (a,b) -> Define a b) defines
    , extraOptions = []
    , cppTmpDir = Nothing
    , inputFile = file
    , outputFile = Nothing
    }
  case a of
    Left e  -> putStrLn "preprocessing error" >> exitWith e
    Right a -> return a
    
