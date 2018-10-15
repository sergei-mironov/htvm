{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module HTVM.IR.Types where

import Text.Show.Deriving (deriveShow1)
import Text.Read.Deriving (deriveRead1)
import Data.Eq.Deriving (deriveEq1)
import Data.Functor.Classes (Show1(..))
import Data.Functor.Foldable (Base, Fix(..), Recursive(..), Corecursive(..), unfix)
import Data.Functor.Foldable.TH (makeBaseFunctor)

type Id = String

data Pat = Pat String
  deriving(Eq,Ord,Show,Read)

data Const =
    ConstR Rational
  | ConstS String
  deriving (Eq,Ord,Show,Read)

eqConst :: Rational -> Const -> Const -> Bool
eqConst tol a b =
  case (a,b) of
    (ConstR af,ConstR bf) -> abs (af-bf) < tol
    (ConstS as,ConstS bs) -> as == bs
    _ -> False

-- | Whitespace container should allow to keep information about comments,
-- linebreaks, etc.
data Whitespaced f a = Whitespaced { cm_get :: Maybe String, cm_next :: (f a) }
  deriving(Eq,Show,Read,Functor)

deriveEq1   ''Whitespaced
deriveShow1 ''Whitespaced
deriveRead1 ''Whitespaced

-- | Weak type annotation, as defined by user. Possibly wrong or incomoplete
data Labeled t f a = Labeled { lb_get :: Maybe t, lb_next :: f a }
  deriving(Eq,Show,Read,Functor)

deriveShow1 ''Labeled
deriveRead1 ''Labeled
deriveEq1   ''Labeled

-- | Strong type annotation, checked by the (missing) typechecker
data Typed t f a = Typed { tpd_get :: t, tpd_next :: f a }
  deriving(Eq,Show,Read,Functor)

deriveShow1 ''Typed
deriveRead1 ''Typed
deriveEq1   ''Typed

-- | Types annotated with shapes
--   a, b, c[3x4], d[3xN], (->)
data Shaped s f a = Shaped { shp_get :: s, shp_next :: f a }
  deriving(Eq,Show,Read,Functor)

deriveShow1 ''Shaped
deriveRead1 ''Shaped
deriveEq1   ''Shaped

data Shape =
    STail
  | SConsI Id Shape
  -- ^ Shape variable
  | SConsC Integer Shape
  -- ^ Shape const
  deriving(Eq,Ord,Show,Read)

makeBaseFunctor ''Shape
deriveShow1 ''ShapeF
deriveRead1 ''ShapeF
deriveEq1   ''ShapeF

type Shape1 = Fix ShapeF
type ShapeW = Fix (Whitespaced ShapeF)

data Type =
    TConst String
  | TIdent String
  | TApp Type Type
  | TLam Pat Type
  deriving(Eq,Ord,Show,Read)

makeBaseFunctor ''Type
deriveShow1 ''TypeF
deriveRead1 ''TypeF
deriveEq1   ''TypeF

type Type1 = Fix TypeF
type TypeW = Fix (Whitespaced TypeF)
type TypeSW = Fix (Shaped ShapeW (Whitespaced TypeF))

-- | Data type representing lambda-calculus expressions.
data Expr =
    Const Const
    -- ^ Constant
  | Ident Id
    -- ^ Bare identifier
  | Lam Pat Expr
    -- ^ A lambda abstraction.
  | Let Pat Expr Expr
    -- ^ Let-binding
  | App Expr Expr
    -- ^ Application
  | Slice Expr [Expr]
    -- ^ Tensor-like slice
  deriving (Eq,Ord,Show,Read)

makeBaseFunctor ''Expr
deriveShow1 ''ExprF
deriveRead1 ''ExprF
deriveEq1   ''ExprF


type Expr1 = Fix ExprF
type ExprTW = Fix (Typed TypeSW (Whitespaced ExprF))
type ExprLW = Fix (Labeled TypeSW (Whitespaced ExprF))

type instance Base (Whitespaced ExprF _) = ExprF
type instance Base (Labeled _ (Whitespaced ExprF) _) = ExprF
type instance Base (Whitespaced TypeF _) = TypeF
type instance Base (Typed _ (Whitespaced ExprF) _) = ExprF
type instance Base (Shaped _ (Whitespaced TypeF) _) = TypeF
type instance Base (Whitespaced ShapeF _) = ShapeF

test1 :: ShapeW ->
        Whitespaced ShapeF (Fix (Whitespaced ShapeF))
test1 v = unfix v

test2 :: TypeSW ->
        Shaped ShapeW (Whitespaced TypeF) (Fix (Shaped ShapeW (Whitespaced TypeF)))
test2 v = unfix v

data Program = Program Expr
  deriving (Show,Read)

