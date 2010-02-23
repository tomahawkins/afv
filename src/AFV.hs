module Main (main) where

import Data.List
import System.Environment
import System.IO

import Compile
import Parse
import Verify

version = "0.1.1"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> help
    (a:_) | elem a ["help", "-h", "--help", "-help"] -> help
          | elem a ["version", "-v", "--version"] -> putStrLn $ "afv " ++ version
    ["example"] -> example
    ["header"]  -> header
    ("verify":args) -> do
      let a = parseArgs Args { yices = "yices", gcc = "gcc", k = 20, defines = [("AFV", "")], includes = ["."], inputs = [] } args
      units <- sequence [ putStrLn ("parsing " ++ f ++ " ...") >> hFlush stdout >> parse (gcc a) (includes a) (defines a) f | f <- inputs a ]
      model <- compile units
      verify (yices a) (k a) model
    _ -> help


data Args = Args
  { yices    :: FilePath
  , k        :: Int
  , gcc      :: FilePath
  , defines  :: [(String, String)]
  , includes :: [FilePath]
  , inputs   :: [FilePath]
  } deriving Show

parseArgs :: Args -> [String] -> Args
parseArgs a b = case b of
  [] -> a
  arg : args | isYices   -> parseArgs a { yices = pathYices } args
             | isGCC     -> parseArgs a { gcc = pathGCC   } args
             | isInclude -> parseArgs a { includes = includes a ++ [drop 2 arg]    } args
             | isDefine  -> parseArgs a { defines  = defines a  ++ [(name, value)] } args
    where
    isYices = isPrefixOf "--yices=" arg
    pathYices = drop 8 arg
    isGCC   = isPrefixOf "--gcc="   arg
    pathGCC = drop 6 arg
    isInclude = length arg > 2 && take 2 arg == "-I"
    isDefine  = length arg > 2 && take 2 arg == "-D"
    (name, value') = span (/= '=') $ drop 2 arg
    value = if length value' <= 1 then "1" else tail value'
  "-k" : k : args -> parseArgs a { k = read k } args
  files -> a { inputs = files }
 

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
  , "  afv - Atom's Formal Verifier"
  , ""
  , "VERSION"
  , "  " ++ version
  , ""
  , "SYNOPSIS"
  , "  afv verify [-k max-k] [--yices=path-to-yices] [--gcc=path-to-gcc] {-Idir} {-Dmacro[=def]} c-file {c-file}"
  , "  afv header"
  , "  afv example"
  , ""
  , "DESCRIPTION"
  , "  Afv performs bounded model checking and k-induction on C code with a signle infinite loop."
  , "  Requires GCC for C preprocessing and the Yices SMT solver."
  , ""
  , "COMMANDS"
  , "  verify [options] c-files ..."
  , "    Runs verification."
  , ""
  , "    -k n          : Max k-induction depth."
  , "    --yices=path  : Path to Yices solver."
  , "    --gcc=path    : Path to GCC."
  , "    -Idir         : Add directory to includes search path."
  , "    -Dmacro       : Define a macro."
  , "    -Dmacro=def   : Define a macro with a value."
  , ""
  , "  header"
  , "    Writes AFV's header file (afv.h), which provides the 'assert' and 'assume' functions."
  , ""
  , "  example"
  , "    Writes an example design (example.c).  Verify with:"
  , "      afv verify -k 15 example.c"
  , ""
  ]
