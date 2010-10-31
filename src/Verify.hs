module Verify (verify) where

import Control.Monad.State hiding (State)
import Language.C.Data.Position
import Math.SMT.Yices.Pipe
import Math.SMT.Yices.Syntax
import System.IO
import Text.Printf

import Error
import Model

-- | Verify a model with k-induction.
verify :: FilePath -> Int -> Model -> IO ()
verify _ maxK _ | maxK < 1 = error "max k can not be less than 1"
verify yices maxK m' = mapM_ (verifyModel yices format maxK) models
  where
  m = typeCheckModel m'
  initPlain = removeAssertions $ initActions m
  models = [ (name, m { initActions = actions,   loopActions = []      }) | (name, actions) <- trimAssertions $ initActions m ]
        ++ [ (name, m { initActions = initPlain, loopActions = actions }) | (name, actions) <- trimAssertions $ loopActions m ]
  format = "verifying %-" ++ show (maximum $ map length $ fst $ unzip models) ++ "s    "

-- | Remove all assertions except one.
trimAssertions :: [Action] -> [(String, [Action])]
trimAssertions [] = []
trimAssertions (a:b) = case a of
  Assert _ n _ -> (n, a : removeAssertions b) : trimAssertions b
  Branch c t f p -> [ (name, Branch c actions (removeAssertions f) p : removeAssertions b)   | (name, actions) <- trimAssertions t ]
                 ++ [ (name, Branch c (removeAssertions t) actions p : removeAssertions b)   | (name, actions) <- trimAssertions f ]
                 ++ [ (name, Branch c (removeAssertions t) (removeAssertions f) p : actions) | (name, actions) <- trimAssertions b ]
  _ -> [ (name, a : actions) | (name, actions) <- trimAssertions b ]

-- | Remove all assertions.
removeAssertions :: [Action] -> [Action]
removeAssertions [] = []
removeAssertions (a:b) = case a of
  Assert _ _ _ -> removeAssertions b
  Branch c t f p -> Branch c (removeAssertions t) (removeAssertions f) p : removeAssertions b
  _ -> a : removeAssertions b

-- | Trim all unneeded stuff from a model.
trimModel :: Model -> Model
trimModel = id --XXX


-- | Verify a trimmed model.
verifyModel :: FilePath -> String -> Int -> (String, Model) -> IO ()
verifyModel y format maxK (name, m') = do
  printf format name
  hFlush stdout
  env0 <- initEnv y m
  execStateT (actions (initActions m) >> doLoop >> check env0 1) env0
  return ()
  where
  m = trimModel m'
  check :: Env -> Int -> Y ()
  check env0 k = do
    actions $ loopActions m
    resultBasis <- checkBasis m env0
    case resultBasis of
      Fail a  -> liftIO (printf "FAILED: disproved basis in k = %d (see trace)\n" k) >> writeTrace True name a
      Problem -> error "Verify.check1"
      Pass | null $ loopActions m -> liftIO $ printf "passed: assertion in initialization\n"
           | otherwise -> do
        resultStep <- checkStep
        case resultStep of
          Fail a | k < maxK  -> check env0 (k + 1)
                 | otherwise -> liftIO (printf "inconclusive: unable to proved step up to max k = %d (see trace)\n" k) >> writeTrace False name a
          Problem -> error "Verify.check2"
          Pass    -> liftIO $ printf "passed: proved step in k = %d\n" k
        
data Result = Pass | Fail [ExpY] | Problem

-- | Check induction step.
checkStep :: Y Result
checkStep = do
  env <- get
  if null $ asserts env
    then return Pass
    else do
      let cmds = vars env ++ loopCmds env ++ [ASSERT $ NOT $ head $ asserts env] ++ [CHECK]
      r <- liftIO $ quickCheckY' (yices env) [] cmds
      return $ result r

-- | Check induction basis.
checkBasis :: Model -> Env -> Y Result
checkBasis m env0 = do
  env <- get
  let cmds = vars env ++ initCmds env ++ loopCmds env ++ [ASSERT $ NOT $ AND $ asserts env]
          ++ [ ASSERT $ VarE (getVar' env0 (State a)) := exprConst t c | a@(VS _ t c _) <- variables m ]
          ++ [CHECK]
  r <- liftIO $ quickCheckY' (yices env) [] cmds
  return $ result r

result :: ResY -> Result
result a = case a of
  Sat a   -> Fail a
  UnSat _ -> Pass
  InCon _ -> Problem
  _ -> error $ "unexpected yices results: " ++ show a


actions :: [Action] -> Y ()
actions a = mapM_ (action $ LitB True) a

assign :: Position -> V -> String -> Y ()
assign p v n = do
  case v of
    State (VS name _ _ _) -> addTrace $ Assign' p name n
    Local name _ _ _      -> addTrace $ Assign' p name n
    _ -> return ()
    
action :: ExpY -> Action -> Y ()
action enabled a = case a of
  Declare v e p -> do
    e <- expr (typeOf v) e
    v' <- addVar v
    addCmd $ ASSERT (VarE v' := e)
    assign p v v'
  Assign v e p -> do
    e <- expr (typeOf v) e
    vLast <- getVar v
    v' <- addVar v
    addCmd $ ASSERT (VarE v' := IF enabled e (VarE vLast))
    assign p v v'
  Assert a n p -> do
    a <- getVar a
    env <- get
    put env { asserts = (enabled :=> VarE a) : asserts env }
    addTrace $ Assert' p n a
  Assume a _ _ -> do
    a <- getVar a
    addCmd $ ASSERT (enabled :=> VarE a)
  Branch a onTrue onFalse p -> do
    a' <- getVar a
    env0 <- get
    if inLoop env0
      then put env0 { loopTrace = [] }
      else put env0 { initTrace = [] }
    mapM_ (action $ AND [enabled,       VarE a']) onTrue
    env1 <- get
    if inLoop env1
      then put env1 { loopTrace = [] }
      else put env1 { initTrace = [] }
    mapM_ (action $ AND [enabled, NOT $ VarE a']) onFalse
    env2 <- get
    if inLoop env2
      then put env2 { loopTrace = Branch' p a' (reverse $ loopTrace env1) (reverse $ loopTrace env2) : loopTrace env0 }
      else put env2 { initTrace = Branch' p a' (reverse $ initTrace env1) (reverse $ initTrace env2) : initTrace env0 }
  


expr :: Type -> E -> Y ExpY
expr t a = case a of
  Var a@(Volatile _ _ _) -> addVar a >>= return . VarE
  Var a   -> getVar a >>= return . VarE
  Const a -> return $ exprConst t a
  Not a _ -> expr Bool a >>= return . NOT

  And a b _ -> do
    a <- expr Bool a
    b <- expr Bool b
    return $ AND [a, b]

  Or  a b _ -> do
    a <- expr Bool a
    b <- expr Bool b
    return $ OR  [a, b]

  Mul a b _ -> do
    a <- expr t a
    b <- expr t b
    return $ a :*: b
  
  Div a b _  -> do
    a <- expr t a
    b <- expr t b
    return $ op a b
    where
    op = case t of
      Integer _ -> DIV
      _         -> (:/:)

  Mod a b _ -> do
    a <- expr t a
    b <- expr t b
    return $ MOD a b

  Add a b _ -> do
    a <- expr t a
    b <- expr t b
    return $ a :+: b

  Sub a b _ -> do
    a <- expr t a
    b <- expr t b
    return $ a :-: b

  Lt  a b n -> do
    let t' = unify n (typeOf a) (typeOf b)
    a <- expr t' a
    b <- expr t' b
    return $ a :< b

  Eq  a b n -> do
    let t' = unify n (typeOf a) (typeOf b)
    a <- expr t' a
    b <- expr t' b
    return $ a := b

  Mux a b c n -> do
    let t' = unify n (typeOf b) (typeOf c)
    a <- expr Bool a
    b <- expr t' b
    c <- expr t' c
    return $ IF a b c

  Ref   _ _ -> notSupported' a "address-of operator"
  Deref _ _ -> notSupported' a "indirection operator"

exprConst :: Type -> Const -> ExpY
exprConst t a = case t of
  --Integer  Nothing -> error "Verify.exprConst1"
  --Rational Nothing -> error "Verify.exprConst2"
  _ -> case a of 
    CBool     a _ -> LitB a
    CInteger  a _ | t == Bool -> LitB $ a /= 0
                  | otherwise -> LitI a
    CRational a _ -> LitR a



type Y = StateT Env IO

data Env = Env
  { yices     :: FilePath
  , nextId    :: Int
  , inLoop    :: Bool
  , names     :: [(V, String)]
  , vars      :: [CmdY]
  , initCmds  :: [CmdY]
  , loopCmds  :: [CmdY]
  , asserts   :: [ExpY]
  , initTrace :: [Trace]
  , loopTrace :: [Trace]
  }
 
data Trace
  = Assign' Position String String
  | Assert' Position String String
  | Branch' Position String [Trace] [Trace] 

initEnv :: FilePath -> Model -> IO Env
initEnv y m = execStateT (mapM_ f $ variables m) env
  where
  env = Env
    { yices     = y
    , nextId    = 0
    , inLoop    = False
    , names     = []
    , vars      = []
    , initCmds  = []
    , loopCmds  = []
    , asserts   = []
    , initTrace = []
    , loopTrace = []
    }
  f :: VS -> Y ()
  f a = do
    n <- addVar v
    assign (posOf v) v n
    where
    v = State a

doLoop :: Y ()
doLoop = do
  env <- get
  put env { inLoop = True }

addVar :: V -> Y String
addVar v = do
  env <- get
  let name = printf "n%d" $ nextId env
  put env { nextId = nextId env + 1, names = (v, name) : names env, vars = DEFINE (name, VarT $ typeY v) Nothing : vars env }
  return name

addCmd :: CmdY -> Y ()
addCmd cmd = do
  env <- get
  if inLoop env
    then put env { loopCmds = cmd : loopCmds env }
    else put env { initCmds = cmd : initCmds env }

addTrace :: Trace -> Y ()
addTrace t = do
  env <- get
  if inLoop env
    then put env { loopTrace = t : loopTrace env }
    else put env { initTrace = t : initTrace env }

getVar :: V -> Y String
getVar v = do
  env <- get
  return $ getVar' env v

getVar' :: Env -> V -> String
getVar' env v = case lookup v $ names env of
  Just a -> a
  Nothing -> error $ "Verify.getVar: variable not found: " ++ show v  ++ " in " ++ show (names env)

typeY :: TypeOf a => a -> String
typeY a = case typeOf a of
  Void       -> error "Verify.typeY: void"
  Ptr _      -> error "Verify.typeY: ptr"
  Bool       -> "bool"
  Integer  _ -> "int"
  Rational _ -> "real"

writeTrace :: Bool -> String -> [ExpY] -> Y ()
writeTrace withInit name table' = do
  env <- get
  let format p = printf ("%-" ++ show (maximum $ map maxPosWidth $ (if withInit then initTrace env else []) ++ loopTrace env) ++ "s    ") $ position p
  liftIO $ writeFile (name ++ ".trace") $ (if withInit then trace format "init" (initTrace env) else "") ++ trace format "loop" (loopTrace env)
  where
  trace :: (Position -> String) -> String -> [Trace] -> String
  trace format n t = n ++ ":\n" ++ concatMap (f format) (reverse t)
  table = [ (n, if v then "true" else "false") | VarE n := LitB v <- table' ]
       ++ [ (n, show v) | VarE n := LitI v <- table' ]
       ++ [ (n, show v) | VarE n := LitR v <- table' ]
  f :: (Position -> String) -> Trace -> String
  f format a = case a of
    Assign' p name var -> case lookup var table of
      Nothing -> ""
      Just value -> format p ++ name ++ " = " ++ value ++ ";\n"
    Assert' p name var -> case lookup var table of
      Just "true"  -> format p ++ "assertion passed: " ++ name ++ "\n"
      Just "false" -> format p ++ "assertion FAILED: " ++ name ++ "\n"
      _ -> ""
    Branch' p cond onTrue onFalse -> case lookup cond table of
      Just "true"  -> format p ++ "branch true\n"  ++ concatMap (f format) onTrue
      Just "false" -> format p ++ "branch false\n" ++ concatMap (f format) onFalse
      _ -> ""

  maxPosWidth :: Trace -> Int
  maxPosWidth a = case a of
    Assign' p _ _ -> length $ position p
    Assert' p _ _ -> length $ position p
    Branch' p _ a b -> maximum $ [length (position p)] ++ map maxPosWidth a ++ map maxPosWidth b
