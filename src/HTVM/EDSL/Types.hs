{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HTVM.EDSL.Types where

import Data.Monoid

newtype Name = Name String
  deriving(Show,Read,Ord,Eq,Semigroup,Monoid)

data Axis =
    GlobalAxis Name
  | LocalAxis Integer
  -- ^ Axis ID within a current tensor
  deriving(Show,Read,Ord,Eq)

data Expr =
    EBinOp Name Expr Expr
  | EUnOp Name Expr
  | ESlice TenExpr [Axis]
  deriving(Show,Read,Ord,Eq)

type Shape = [Axis]

newtype Type = Type Name
  deriving(Show,Read,Ord,Eq,Semigroup,Monoid)

float32 = Type "float32"

-- | Common arguments to various functions
data Args = Args {
    a_name :: Maybe Name
  , a_shape :: Maybe Shape
  , a_type :: Maybe Type
  } deriving(Show,Read,Ord,Eq)

data Tensor = Tensor {
    t_args :: Args
  , t_expr :: Expr
  } deriving(Show,Read,Ord,Eq)

nullArgs :: Args
nullArgs = Args mempty mempty mempty

-- | Tensor Expressions. Allow us to write code like
-- `Tensor a,b; Tensor c = a + b;`
data TenExpr =
    TenPlh Placeholder
  | TenLet Tensor TenExpr TenExpr
  | TenCompute Args Expr
  | TenBinOp TenExpr TenExpr
  | TenUnOp TenExpr TenExpr
  | TenCall { tc_fname::Name, tc_attrs::Args, tc_args::[TenExpr] }
    -- ^ Function call. `tc_fname` is the name of a function. `tc_attrs` is
    -- common non-Tensor arguments to this function. `tc_args` is the Tensor
    -- arguments.
  deriving(Show,Read,Ord,Eq)


type Placeholder = (Name,Type,Shape)

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

