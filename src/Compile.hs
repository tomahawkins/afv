module Compile (compile) where

import Control.Monad.State hiding (State)
import Data.List
import Language.C
import Language.C.Data.Ident

import Error
import Model hiding (CInteger)
import qualified Model as M
import Utils

-- | Compiles a program to a model for analysis.
compile :: CTranslUnit -> IO Model
compile unit = do
  m <- execStateT (evalStat initEnv $ rewrite unit) initMDB
  return (model m) { initActions = reverse $ initActions (model m), loopActions = reverse $ loopActions (model m) }

none :: NodeInfo
none = internalNode

-- | Rewrites a program to a single statement.  Requires no recursive functions and unique declarations for all top level declarations.
rewrite :: CTranslUnit -> CStat
rewrite (CTranslUnit items _) = CCompound [] (map f items ++ [CBlockStmt callMain]) none
  where
  f (CDeclExt a) = CBlockDecl a
  f (CFDefExt a) = CNestedFunDef a
  f a@(CAsmExt _) = notSupported a "inline assembly"
  callMain :: CStat
  callMain = CExpr (Just $ CCall (CVar (Ident "main" 0 none) none) [] none) none

type M = StateT MDB IO

data MDB = MDB
  { nextId' :: Int
  , stage   :: Stage
  , stack   :: [Ident]
  , model   :: Model
  }

data Stage = Init | Loop | Done deriving Eq

initMDB :: MDB
initMDB = MDB
  { nextId' = 0
  , stage   = Init
  , stack   = []
  , model   = Model { initActions = [], loopActions = [] }
  }

nextId :: M Int
nextId = do
  m <- get
  put m { nextId' = nextId' m + 1 }
  return $ nextId' m

setStage :: Stage -> M ()
setStage a = do
  m <- get
  put m { stage = a }

getStage :: M Stage
getStage = do
  m <- get
  return $ stage m

-- | Environment for resolving identifiers.
data Env = Env
  { function :: Ident -> Function
  , variable :: Ident -> V
  }

data Function
  = Function Env CFunDef
  | Primitive Primitive

data Primitive
  = Assert
  | Assume

-- | Creates a branch.
branch :: Position -> V -> M () -> M () -> M ()
branch p a onTrue onFalse = do   --XXX What happens if infinite loop is called within branch?
  m1 <- get
  case stage m1 of
    Init -> put m1 { model = (model m1) { initActions = [] } }
    Loop -> put m1 { model = (model m1) { loopActions = [] } }
    Done -> unexpected' p "statements after infinite loop"
  onTrue
  m2 <- get
  case stage m2 of
    Init | stage m1 == Init -> put m1 { model = (model m2) { initActions = [] } }
    Loop | stage m1 == Loop -> put m1 { model = (model m2) { loopActions = [] } }
    _ -> unexpected' p "infinite loop in branch"
  onFalse
  m3 <- get
  case stage m3 of
    Init | stage m1 == Init && stage m2 == Init -> do
      put m3 { model = (model m3) { initActions = initActions $ model m1 } }
      newAction $ Branch a (reverse $ initActions $ model m2) (reverse $ initActions $ model m3) p
    Loop | stage m1 == Loop && stage m2 == Loop -> do
      put m3 { model = (model m3) { loopActions = loopActions $ model m1 } }
      newAction $ Branch a (reverse $ loopActions $ model m2) (reverse $ loopActions $ model m3) p
    _ -> unexpected' p "infinite loop in branch"

-- | Push an identifier onto the call stack, do something, then pop it off.
callStack :: Ident -> M a -> M a
callStack id a = do
  m <- get
  put m { stack = id : stack m }
  a <- a
  m <- get
  put m { stack = tail $ stack m }
  return a

callPath :: M String
callPath = do
  m <- get
  let s = stack m
  return $ intercalate "." [ n | Ident n _ _ <- reverse $ tail s ]

-- | The initial environment defines the assert and assume functions.
initEnv :: Env
initEnv = Env
  { function = \ a@(Ident name _ _) -> case name of
      "assert" -> Assert
      "assume" -> Assume
      _        -> unexpected "function not found" a
  , variable = \ a -> unexpected "variable not found" a
  }

{-
  [EnvFunction "assert" $ Function 1 assert, EnvFunction "assume" $ Function 1 assume]
  where
  assert a' = do
    let a = head a'
    s <- callPath
    x <- latchBool (posOf a) a
    newAction $ Assert x s (posOf a)
  assume a' = do
    let a = head a'
    s <- callPath
    x <- latchBool (posOf a) a
    newAction $ Assume x s (posOf a)
-}

-- | Adds new action.
newAction :: Action -> M ()
newAction a = do
  m <- get
  case stage m of
    Init -> put m { model = (model m) { initActions = a : initActions (model m) }}
    Loop -> put m { model = (model m) { loopActions = a : loopActions (model m) }}
    Done -> error "Compile.newAction"

-- | Adds new variable.
addVar :: Env -> V -> M Env
addVar env a = return $ EnvVariable name a : env
  where
  name = case a of
    State (VS a _ _ _)  -> a
    Volatile a _ _   -> a
    Local    a _ _ _ -> a
    _ -> error "Compile.addVar: should not call addVar with Tmp or Branch"



evalStat :: Env -> CStat -> M ()
evalStat env a = do
  stage <- getStage
  when (stage == Done) $ unexpected a "statements after infinite loop"
  case a of
    CFor (Left Nothing) Nothing Nothing a _              | stage == Init -> do setStage Loop >> evalStat env a >> setStage Done
    CWhile (CConst (CIntConst (CInteger 1 _ _) _)) a _ _ | stage == Init -> do setStage Loop >> evalStat env a >> setStage Done
    CLabel i a [] _ -> callStack i $ evalStat env a
    CCompound ids items _ -> f ids
      where
      f :: [Ident] -> M ()
      f [] = foldM evalBlockItem env items >> return ()
      f (a:b) = callStack a $ f b
    CIf a b Nothing n -> evalStat env $ CIf a b (Just $ CCompound [] [] n) n
    CIf a b (Just c) n -> do
      a <- latchBool (posOf n) $ evalExpr env a
      branch (posOf n) a (evalStat env b) (evalStat env c)
      return ()

    CExpr Nothing _ -> return ()
    CExpr (Just (CAssign op a b n)) _ -> case op of
      CAssignOp -> case evalExpr env a of
        Var v -> assign (posOf n) v $ evalExpr env b
        _ -> unexpected a "non variable in left hand of assignment"
      CMulAssOp -> f CMulOp
      CDivAssOp -> f CDivOp
      CRmdAssOp -> f CRmdOp
      CAddAssOp -> f CAddOp
      CSubAssOp -> f CSubOp
      CShlAssOp -> f CShlOp
      CShrAssOp -> f CShrOp
      CAndAssOp -> f CAndOp
      CXorAssOp -> f CXorOp
      COrAssOp  -> f COrOp
      where
      f :: CBinaryOp -> M ()
      f op = evalStat env (CExpr (Just (CAssign CAssignOp a (CBinary op a b n) n)) n)

    CExpr (Just (CCall (CVar f _) args _)) _ -> do
      when (arity /= length args) $ unexpected' f $ "function called with " ++ show (length args) ++ " arguments, but defined with " ++ show arity ++ " arguments"
      callStack f $ func $ map (evalExpr env) args
      where
      Function arity func = function env f

    CExpr (Just (CCall _ _ _)) _ -> notSupported a "non named function references"

    CExpr (Just (CUnary op a n1)) n2 | elem op [CPreIncOp, CPostIncOp] -> evalStat env (CExpr (Just (CAssign CAddAssOp a one n1)) n2)
                                     | elem op [CPreDecOp, CPostDecOp] -> evalStat env (CExpr (Just (CAssign CSubAssOp a one n1)) n2)
      where
      one = CConst $ CIntConst (cInteger 1) n1

    _ -> notSupported a "statement"

evalBlockItem :: Env -> CBlockItem -> M Env
evalBlockItem env a = case a of
  CBlockStmt a    -> evalStat env a >> return env
  CBlockDecl a    -> evalDecl env a
  CNestedFunDef a -> evalFunc env a

-- No stateful operations for expressions.
evalExpr :: Env -> CExpr -> E
evalExpr env a = case a of
  CCond a (Just b) c n -> Mux (evalExpr env a) (evalExpr env b) (evalExpr env c) $ posOf n
  CCond a Nothing  b n -> Or (evalExpr env a) (evalExpr env b) $ posOf n
  CBinary op a' b' n -> case op of
    CMulOp -> Mul a b p
    CDivOp -> Div a b p
    CRmdOp -> Mod a b p
    CAddOp -> Add a b p
    CSubOp -> Sub a b p
    CShlOp -> notSupported' a "(<<)"
    CShrOp -> notSupported' a "(>>)"
    CLeOp  -> Lt a b p
    CGrOp  -> Lt b a p
    CLeqOp -> Not (Lt b a p) p
    CGeqOp -> Not (Lt a b p) p
    CEqOp  -> Eq a b p
    CNeqOp -> Not (Eq a b p) p
    CAndOp -> notSupported' a "(&)"
    CXorOp -> notSupported' a "(^)"
    COrOp  -> notSupported' a "(|)"
    CLndOp -> And a b p
    CLorOp -> Or a b p
    where
    a = evalExpr env a'
    b = evalExpr env b'
    p = posOf n

  CUnary op a' n -> case op of
    CPlusOp -> a
    CMinOp  -> Sub zero a p
    CNegOp  -> Not a p
    --(CAdrOp,  a) -> return $ Ref a p
    --(CIndOp,  a) -> return $ Deref a p
    _ -> notSupported' n "unary operator"
    where
    a = evalExpr env a'
    p = posOf n
    zero = Const $ M.CInteger 0 p

  CVar i _ -> Var $ variable env i
  CConst a -> case a of
    CIntConst (CInteger a _ _) n -> Const $ M.CInteger a $ posOf n
    CFloatConst (CFloat a) n -> Const $ CRational (toRational (read a :: Double)) $ posOf n
    _ -> notSupported a "char or string constant"
  _ -> notSupported a "expression"
    
evalDecl :: Env -> CDecl -> M Env
evalDecl env d@(CDecl specs decls _) = if isExtern typInfo then return env else foldM evalDecl' env decls
  where
  (typInfo, typ) = typeInfo specs
  evalDecl' :: Env -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> M Env
  evalDecl' env (a, b, c) = case a of
    Just (CDeclr (Just i@(Ident name _ n)) [] Nothing [] _) -> case (b, c) of
      (Nothing, Nothing) -> evalDecl' env (a, Just $ CInitExpr (CConst (CIntConst (cInteger 0) n)) n, Nothing)
      (Just (CInitExpr (CConst const) n'), Nothing) | isStatic typInfo && not (isVolatile typInfo) -> addVar env v
        where
        v = State $ VS name typ init $ posOf n
        init = case typ of
          Void -> unexpected d "void type for variable declaration"
          Ptr _ -> notSupported d "pointer types"
          Bool -> CBool (cInt /= 0) $ posOf n'
          Integer _  -> M.CInteger cInt $ posOf n'
          Rational _ -> CRational cRat $ posOf n'

        cInt :: Integer
        cInt = case const of
          CIntConst (CInteger a _ _) _ -> a
          _ -> unexpected const "non integer initialization"

        cRat :: Rational
        cRat = case const of
          CIntConst (CInteger a _ _) _ -> fromIntegral a
          CFloatConst (CFloat a) _     -> fromIntegral (read a :: Integer)
          _ -> unexpected const "non numeric initialization"

      (Just (CInitExpr c _), Nothing) -> evalDecl' env (a, Nothing, Just c)

      (Nothing, Just e) -> do
        v <- if isVolatile typInfo
          then return $ Volatile name typ $ posOf n
          else do
            i <- nextId
            return $ Local name typ i p
        declare p v $ evalExpr env e
        addVar env v
        where
        p = posOf e
      _ -> notSupported' i "variable declaration"
    _ -> notSupported' d "arrays, pointers, or functional pointers (So what good is this tool anyway?)"
      

evalFunc :: Env -> CFunDef -> M Env
evalFunc env f =  do
  when (typ /= Void) $ notSupported f "non void function"
  return $ EnvFunction name (Function (length args) func) : env
  where
  (specs, (Ident name _ _), args', stat) = functionInfo f
  (_, typ) = typeInfo specs
  args = concatMap funcArgs args'
  func args' = do
    env <- foldM bindArg env $ zip args args'
    evalStat env stat

bindArg :: Env -> ((Ident, (TypeInfo, Type)), E) -> M Env
bindArg env ((Ident name _ n, (typInfo, typ)), a) = if isVolatile typInfo
  then addVar env (Volatile name typ (posOf n))
  else do
    i <- nextId
    let v = Local name typ i (posOf n)
    declare (posOf n) v a
    addVar env v

funcArgs :: CDecl -> [(Ident, (TypeInfo, Type))]
funcArgs (CDecl specs decls n) = map f decls
  where
  t = typeInfo specs
  f (Just (CDeclr (Just i) [] Nothing [] _), Nothing, Nothing) = (i, t)
  f _ = notSupported' n "function argument"




-- | Declares a new variable with a value.
declare :: Position -> V -> E -> M ()
declare n v a = case v of
  Volatile _ _ _ -> return ()
  _ -> newAction $ Declare v a n

-- | Assigns a value to an existing variable.
assign :: Position -> V -> E -> M ()
assign n v a = case v of
  Volatile _ _ _ -> return ()
  _ -> newAction $ Assign v a n

-- | Latch a value at a point in time.
latchBool :: Position -> E -> M V
latchBool n a = do
  i <- nextId
  let v = Tmp Bool i n
  declare n v a
  return v

-- | Extract relavent info from a function declaration.
functionInfo :: CFunDef -> ([CDeclSpec], Ident, [CDecl], CStat)
functionInfo (CFunDef specs (CDeclr (Just ident) [(CFunDeclr (Right (args, False)) _ _)] Nothing [] _) [] stmt _) = (specs, ident, args, stmt)  --XXX What is the False?
functionInfo f = notSupported f "function"

