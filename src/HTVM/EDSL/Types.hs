{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HTVM.EDSL.Types where

import Data.Monoid

newtype Name = Name { n_get :: String }
  deriving(Show,Read,Ord,Eq,Semigroup,Monoid)

data Axis =
    GlobalAxis Name
  | LocalAxis Integer
  -- ^ Axis ID within a current tensor
  deriving(Show,Read,Ord,Eq)

-- | Shape variable, unknown at compile time, but determined at compile time
data ShapeVar = ShapeVar Name
  deriving(Show,Read,Ord,Eq)

data Const =
    CInt Integer
  | CFloat32 Float
  deriving(Show,Read,Ord,Eq)

-- | Scalar expressions
data Expr =
    EConst Const
  -- ^ Plain constant
  | EShapeVar ShapeVar
  -- ^ Shape variable, unknown at compile time
  | EAxis Axis
  -- ^ Axis placeholder to be used inside the `compute` operator
  | ECall Name [Expr]
  -- ^ Call of a function or an operator
  | ESlice TenExpr [Expr]
  deriving(Show,Read,Ord,Eq)

instance Num Expr where
  (+) a b = ECall (Name "+") [a,b]
  (-) a b = ECall (Name "-") [a,b]
  (*) a b = ECall (Name "*") [a,b]
  negate a = ECall (Name "-") [a]
  abs = error "abs is undefined"
  signum = error "signum is undefined"
  fromInteger = EConst . CInt

-- | Set of Axes, see `compute`
type Axes = [Axis]

-- | A set of expressions. Usually only Consts and ShapeVars are allowed here
type Shape = [Expr]

shape :: [Integer] -> Shape
shape = map (EConst . CInt)

newtype Type = Type Name
  deriving(Show,Read,Ord,Eq,Semigroup,Monoid)

float32 = Type (Name "float32")

-- | Common arguments to various functions
data Args = Args {
    a_name :: Maybe Name
  , a_shape :: Maybe Shape
  , a_type :: Maybe Type
  } deriving(Show,Read,Ord,Eq)

-- | Pattern is a name of Tensor Expression
data Pattern = Pattern {
    p_name :: Name
  } deriving(Show,Read,Ord,Eq)

nullArgs :: Args
nullArgs = Args mempty mempty mempty

-- | Tensor Expressions. Allow us to write code like
-- `Tensor a,b; Tensor c = a + b;`
data TenExpr =
    TenPlh Placeholder
  -- ^ FIXME: Should it be one with TenId? Shape,Name,Type may be moved to
  -- context out of the base AST
  | TenId Pattern
  -- ^ FIXME: Should it be united with TenPlh?
  | TenLet Pattern TenExpr TenExpr
  | TenCompute Args Expr
  -- | TenBinOp TenExpr TenExpr
  -- | TenUnOp TenExpr TenExpr
  | TenCall { tc_fname::Name, tc_attrs::Args, tc_args::[TenExpr] }
  -- ^ Function call. `tc_fname` is the name of a function. `tc_attrs` is
  -- common non-Tensor arguments to this function. `tc_args` is the Tensor
  -- arguments.
  deriving(Show,Read,Ord,Eq)


type Placeholder = (Name,Type,Shape)

pls_name :: Placeholder -> Name
pls_name (nm,_,_) = nm

data Function = Function {
    fun_name :: Name
  , fun_pls :: [Placeholder]
  , fun_body :: TenExpr
  }
  deriving(Show,Read,Ord,Eq)

data Module = Module {
    mod_name :: Name
  , mod_funcs :: [Function]
  }
  deriving(Show,Read,Ord,Eq)

