module Main (main) where

import qualified Data.ByteString.Char8 as B
import Language.C
import System.Environment

import Compile
import Verify

version = "0.2.0"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> help
    (a:_) | elem a ["help", "-h", "--help", "-help"] -> help
          | elem a ["version", "-v", "--version"] -> putStrLn $ "afv " ++ version
    ["example"] -> example
    ["header"]  -> header
    ["verify", file] -> do
      a <- if file == "-" then getContents else readFile file
      model <- compile $ parse (if file == "-" then "stdin" else file) a
      verify "yices" 20 model
    _ -> help

parse :: String -> String -> CTranslUnit
parse name code = case parseC (B.pack code) (initPos name) of
    Left e  -> error $ "parsing error: " ++ show e
    Right a -> a

header :: IO ()
header = do
  putStrLn "writing AFV header file (afv.h) ..."
  writeFile "afv.h" $ unlines
    [ "#ifndef AFV"
    , "#include <assert.h>"
    , "#define assume assert"
    , "#endif"
    ]

example :: IO ()
example = do
  header
  putStrLn "writing example design (example.c) ..."
  putStrLn "verify with:  afv verify -k 15 example.c"
  writeFile "example.c" $ unlines
    [ "// Provides assert and assume functions."
    , "#include \"afv.h\""
    , ""
    , "// Periodic function for verification.  Implements a simple rolling counter."
    , "void example () {"
    , ""
    , "  // The rolling counter."
    , "  static int counter = 0;"
    , ""
    , "  // Assertions."
    , "  GreaterThanOrEqualTo0: assert(counter >= 0);    // The 'counter' must always be greater than or equal to 0."
    , "  LessThan10:            assert(counter < 10);    // The 'counter' must always be less than 10."
    , ""
    , "  // Implementation 1."
    , "  if (counter == 10)"
    , "    counter = 0;"
    , "  else"
    , "    counter++;"
    , ""
    , "  // Implementation 2."
    , "  // if (counter == 9)"
    , "  //   counter = 0;"
    , "  // else"
    , "  //   counter = counter + 1;"
    , ""
    , "  // Implementation 3."
    , "  // if (counter >= 9)"
    , "  //   counter = 0;"
    , "  // else"
    , "  //   counter++;"
    , ""
    , "  // Implementation 4."
    , "  // if (counter >= 9 || counter < 0)"
    , "  //   counter = 0;"
    , "  // else"
    , "  //   counter++;"
    , ""
    , "  // Implementation 5."
    , "  // counter = (counter + 1) % 10;"
    , ""
    , "}"
    , ""
    , "void main() {"
    , "  while (1) example();"
    , "}"
    , ""
    ]



help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  afv - Atom Formal Verifier"
  , ""
  , "VERSION"
  , "  " ++ version
  , ""
  , "SYNOPSIS"
  , "  afv verify ( <file> | - )"
  , "  afv header"
  , "  afv example"
  , ""
  , "DESCRIPTION"
  , "  Afv performs bounded model checking and k-induction on C code with a signle infinite loop."
  , "  Requires GCC for C preprocessing and the Yices SMT solver."
  , ""
  , "COMMANDS"
  , "  verify ( <file> | - )"
  , "    Runs verification on a C preprocessed file (CPP) or from stdin."
  , ""
  , "  header"
  , "    Writes AFV's header file (afv.h), which provides the 'assert' and 'assume' functions."
  , ""
  , "  example"
  , "    Writes an example design (example.c).  Verify with:"
  , "      afv verify -k 15 example.c"
  , ""
  ]
