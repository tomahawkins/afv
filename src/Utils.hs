module Utils
  ( TypeInfo (..)
  , typeInfo
  ) where

import Language.C

import Error
import Model

data TypeInfo = TypeInfo
  { isConst
  , isVolatile
  , isStatic
  , isExtern    :: Bool
  }

typeInfo :: [CDeclSpec] -> (TypeInfo, Type)
typeInfo specs = (info, typ)
  where
  info = foldl f TypeInfo { isConst = False, isVolatile = False, isStatic = False, isExtern = False } specs
  f :: TypeInfo -> CDeclSpec -> TypeInfo
  f t a = case a of
    CStorageSpec a -> case a of
      CStatic _ -> t { isStatic = True }
      CExtern _ -> t { isExtern = True }
      _         -> notSupported a $ "storage class"
    CTypeQual a -> case a of
      CConstQual _ -> t { isConst    = True }
      CVolatQual _ -> t { isVolatile = True }
      _         -> notSupported a $ "type qualifier"
    CTypeSpec _ -> t
  voids    = [ a | CTypeSpec a@(CVoidType _) <- specs ]
  signed   = [ a | CTypeSpec a@(CSignedType _) <- specs ]
  unsigned = [ a | CTypeSpec a@(CUnsigType  _) <- specs ]
  bools    = [ a | CTypeSpec a@(CBoolType _) <- specs ]
  chars    = [ a | CTypeSpec a@(CCharType _) <- specs ]
  shorts   = [ a | CTypeSpec a@(CShortType _) <- specs ]
  longs    = [ a | CTypeSpec a@(CLongType _) <- specs ]
  ints     = [ a | CTypeSpec a@(CIntType _) <- specs ]
  floats   = [ a | CTypeSpec a@(CFloatType _) <- specs ]
  doubles  = [ a | CTypeSpec a@(CDoubleType _) <- specs ]
  typ = case (length voids, length signed, length unsigned, length bools, length chars, length shorts, length longs, length ints, length floats, length doubles) of
    (1, 0, 0, 0, 0, 0, 0, 0, 0, 0) -> Void

    (0, 0, 0, 1, 0, 0, 0, 0, 0, 0) -> Bool

    (0, 0, 0, 0, 1, 0, 0, 0, 0, 0) -> Integer $ Just S8
    (0, 1, 0, 0, 1, 0, 0, 0, 0, 0) -> Integer $ Just S8
    (0, 0, 1, 0, 1, 0, 0, 0, 0, 0) -> Integer $ Just U8

    (0, 0, 0, 0, 0, 1, 0, 0, 0, 0) -> Integer $ Just S16
    (0, 1, 0, 0, 0, 1, 0, 0, 0, 0) -> Integer $ Just S16
    (0, 0, 1, 0, 0, 1, 0, 0, 0, 0) -> Integer $ Just U16
    (0, 0, 0, 0, 0, 1, 0, 1, 0, 0) -> Integer $ Just S16
    (0, 1, 0, 0, 0, 1, 0, 1, 0, 0) -> Integer $ Just S16
    (0, 0, 1, 0, 0, 1, 0, 1, 0, 0) -> Integer $ Just U16

    (0, 0, 0, 0, 0, 0, 0, 1, 0, 0) -> Integer $ Just S32
    (0, 1, 0, 0, 0, 0, 0, 1, 0, 0) -> Integer $ Just S32
    (0, 0, 1, 0, 0, 0, 0, 1, 0, 0) -> Integer $ Just U32
    (0, 0, 0, 0, 0, 0, 1, 0, 0, 0) -> Integer $ Just S32
    (0, 1, 0, 0, 0, 0, 1, 0, 0, 0) -> Integer $ Just S32
    (0, 0, 1, 0, 0, 0, 1, 0, 0, 0) -> Integer $ Just U32
    (0, 0, 0, 0, 0, 0, 1, 1, 0, 0) -> Integer $ Just S32
    (0, 1, 0, 0, 0, 0, 1, 1, 0, 0) -> Integer $ Just S32
    (0, 0, 1, 0, 0, 0, 1, 1, 0, 0) -> Integer $ Just U32

    (0, 0, 0, 0, 0, 0, 2, 0, 0, 0) -> Integer $ Just S64
    (0, 1, 0, 0, 0, 0, 2, 0, 0, 0) -> Integer $ Just S64
    (0, 0, 1, 0, 0, 0, 2, 0, 0, 0) -> Integer $ Just U64
    (0, 0, 0, 0, 0, 0, 2, 1, 0, 0) -> Integer $ Just S64
    (0, 1, 0, 0, 0, 0, 2, 1, 0, 0) -> Integer $ Just S64
    (0, 0, 1, 0, 0, 0, 2, 1, 0, 0) -> Integer $ Just U64

    (0, 0, 0, 0, 0, 0, 0, 0, 1, 0) -> Rational $ Just Float
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 1) -> Rational $ Just Double

    a -> notSupported (head specs) $ "type signature: " ++ show a

