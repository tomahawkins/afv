module Model
  ( Type        (..)
  , IntegerType (..)
  , FloatType   (..)
  , TypeOf      (..)
  , Const       (..)
  , VS          (..)
  , V           (..)
  , E           (..)
  , Action      (..)
  , Model       (..)
  , true
  , false
  , isRational
  , isInteger
  , typeCheckModel
  , unify
  , eVars
  , variables
  ) where

import Data.List
import Language.C.Data.Position

import Error

data IntegerType = U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 deriving (Show, Eq)

data FloatType = Float | Double deriving (Show, Eq)

data Type
  = Void
  | Ptr Type
  | Bool
  | Integer  (Maybe IntegerType)
  | Rational (Maybe FloatType)
  deriving Eq

isInteger :: Type -> Bool
isInteger (Integer _) = True
isInteger _ = False

isRational :: Type -> Bool
isRational (Rational _) = True
isRational _ = False

instance Show Type where
  show a = case a of
    Void -> "void"
    Ptr a -> show a ++ " *"
    Bool -> "bool"
    Integer Nothing -> "unspecified integer type"
    Integer (Just a) -> case a of
      U8  -> "unsigned char"
      U16 -> "unsigned short"
      U32 -> "unsigned long"
      U64 -> "unsigned long long"
      S8  -> "signed char"
      S16 -> "signed short"
      S32 -> "signed long"
      S64 -> "signed long long"
    Rational Nothing -> "unspecified floating type"
    Rational (Just Double) -> "double"
    Rational (Just Float)  -> "float"

class TypeOf a where typeOf :: a -> Type

instance TypeOf Type where typeOf = id

data Const
  = CBool     Bool     Position
  | CInteger  Integer  Position
  | CRational Rational Position
  deriving (Show, Eq)

instance TypeOf Const where
  typeOf a = case a of
    CBool     _ _ -> Bool
    CInteger  _ _ -> Integer  Nothing
    CRational _ _ -> Rational Nothing

instance Pos Const where
  posOf a = case a of
    CBool     _ a -> a
    CInteger  _ a -> a
    CRational _ a -> a

-- | User defined state variable (non-volatile top level variables or non-volatile static variables in functions).
data VS = VS String Type Const Position deriving (Show, Eq)

instance TypeOf VS where typeOf (VS _ t _ _) = t
instance Pos VS where posOf (VS _ _ _ n) = n

-- | Variables.
data V
  = State VS
  | Volatile String Type     Position
  | Local    String Type Int Position
  | Tmp             Type Int Position
  deriving (Show, Eq)
     
instance TypeOf V where
  typeOf a = case a of
    State a -> typeOf a
    Volatile _ a _   -> a
    Local    _ a _ _ -> a
    Tmp      a _ _   -> a

instance Pos V where
  posOf a = case a of
    State a -> posOf a
    Volatile _ _ n -> n
    Local _ _ _ n -> n
    Tmp _ _ n -> n

-- | Expressions.
data E
  = Var V
  | Const Const
  | Not E Position
  | And E E Position
  | Or  E E Position
  | Mul E E Position
  | Div E E Position
  | Mod E E Position
  | Add E E Position
  | Sub E E Position
  | Lt  E E Position
  | Eq  E E Position
  | Mux E E E Position
  | Ref   E Position
  | Deref E Position
  deriving (Show, Eq)

instance Pos E where
  posOf a = case a of
    Var a -> posOf a
    Const a -> posOf a
    Not _ a -> a
    And _ _ a -> a
    Or  _ _ a -> a
    Mul _ _ a -> a
    Div _ _ a -> a
    Mod _ _ a -> a
    Add _ _ a -> a
    Sub _ _ a -> a
    Lt  _ _ a -> a
    Eq  _ _ a -> a
    Mux _ _ _ a -> a
    Ref _ a -> a
    Deref _ a -> a

instance TypeOf E where
  typeOf a = case a of
    Var a       -> typeOf a
    Const a     -> typeOf a
    Not a _     -> unify' Bool (typeOf a)
    And a b _   -> unify' Bool $ unify' (typeOf a) (typeOf b)
    Or  a b _   -> unify' Bool $ unify' (typeOf a) (typeOf b)
    Mul a b _   -> unify' (typeOf a) (typeOf b)
    Div a b _   -> unify' (typeOf a) (typeOf b)
    Mod a b _   -> unify' (typeOf a) (typeOf b)
    Add a b _   -> unify' (typeOf a) (typeOf b)
    Sub a b _   -> unify' (typeOf a) (typeOf b)
    Lt  a b _   -> seq (unify' (typeOf a) (typeOf b)) Bool
    Eq  a b _   -> seq (unify' (typeOf a) (typeOf b)) Bool
    Mux a b c _ -> seq (unify' Bool (typeOf a)) $ unify' (typeOf b) (typeOf c)
    Ref a _ -> Ptr $ typeOf a
    Deref a p -> case typeOf a of
      Ptr a -> a
      t -> err p $ "type violation: expecting pointer type, but got " ++ show t
    where
    unify' = unify a

unify :: Pos a => a -> Type -> Type -> Type
unify n a b = case (a, b) of
  (a, b) | a == b -> a

  (Bool, Integer Nothing) -> Bool
  (Integer Nothing, Bool) -> Bool

  (Integer Nothing, a@(Integer _)) -> a
  (a@(Integer _), Integer Nothing) -> a

  (Integer Nothing, a@(Rational _)) -> a
  (a@(Rational _), Integer Nothing) -> a

  (Rational Nothing, a@(Rational _)) -> a
  (a@(Rational _), Rational Nothing) -> a

  (a, b) -> err n $ "type violation: " ++ show a ++ " incompatiable with " ++ show b

typeCheckModel :: Model -> Model
typeCheckModel m = m { initActions = map typeCheckAction $ initActions m, loopActions = map typeCheckAction $ loopActions m }

typeCheckAction :: Action -> Action
typeCheckAction a = case a of
  Declare v a p  -> seq (unify a (typeOf v) (typeOf a)) $ Declare v a p
  Assign v a p   -> seq (unify a (typeOf v) (typeOf a)) $ Assign  v a p
  Assert a n p   -> seq (unify a Bool (typeOf a))       $ Assert a n p
  Assume a n p   -> seq (unify a Bool (typeOf a))       $ Assume a n p
  Branch a b c p -> seq (unify a Bool (typeOf a))       $ Branch a (map typeCheckAction b) (map typeCheckAction c) p

true :: E
true = Const $ CBool True nopos

false :: E
false = Const $ CBool False nopos

eVars :: E -> [V]
eVars a = case a of
  Var a -> [a]
  Const _ -> []
  Not a _ -> eVars a
  And a b _ -> eVars a ++ eVars b
  Or  a b _ -> eVars a ++ eVars b
  Mul a b _ -> eVars a ++ eVars b
  Div a b _ -> eVars a ++ eVars b
  Mod a b _ -> eVars a ++ eVars b
  Add a b _ -> eVars a ++ eVars b
  Sub a b _ -> eVars a ++ eVars b
  Lt  a b _ -> eVars a ++ eVars b
  Eq  a b _ -> eVars a ++ eVars b
  Mux a b c _ -> eVars a ++ eVars b ++ eVars c
  Ref a _ -> eVars a
  Deref a _ -> eVars a

instance Pos Position where posOf = id

data Action
  = Declare V E Position
  | Assign  V E Position
  | Assert  V String Position
  | Assume  V String Position
  | Branch  V [Action] [Action] Position
  deriving (Show, Eq)

instance Pos Action where
  posOf a = case a of
    Declare _ _ a -> a
    Assign  _ _ a -> a
    Assert  _ _ a -> a
    Assume  _ _ a -> a
    Branch  _ _ _ a -> a

variables :: Model -> [VS]
variables m = nub $ concatMap vars $ initActions m ++ loopActions m
  where
  vars :: Action -> [VS]
  vars a = case a of
    Declare v e _ -> var v ++ [ a | State a <- eVars e ]
    Assign  v e _ -> var v ++ [ a | State a <- eVars e ]
    Assert  v _ _ -> var v
    Assume  v _ _ -> var v
    Branch  v a b _ -> var v ++ concatMap vars a ++ concatMap vars b
  var :: V -> [VS]
  var (State a) = [a]
  var _         = []
    
data Model = Model
  { initActions :: [Action]
  , loopActions :: [Action]
  } deriving Show

