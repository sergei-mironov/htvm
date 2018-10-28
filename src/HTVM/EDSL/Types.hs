{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HTVM.EDSL.Types where

import Data.Monoid
import Data.Text(Text)

-- | Name is the string convertable to valid C identifier
newtype Name = Name { n_get :: Text }
  deriving(Show,Read,Ord,Eq,Semigroup,Monoid)

data Const =
    CInt Integer
  | CFloat32 Float
  deriving(Show,Read,Ord,Eq)

-- | Dimention expression represents the length of the vectors, number of
-- rows/columns in a matrix, etc.
data DimExpr =
    DimConst Integer
  | DimId Pattern
  | DimCall Name [DimExpr]
  deriving(Show,Read,Ord,Eq)

instance Num DimExpr where
  (+) a b = DimCall (Name "+") [a,b]
  (-) a b = DimCall (Name "-") [a,b]
  (*) a b = DimCall (Name "*") [a,b]
  negate a = DimCall (Name "-") [a]
  abs = error "abs is undefined for DimExpr"
  signum = error "signum is undefined for DimExpr"
  fromInteger = DimConst

-- | Shape expressions represents the shape of a tensor, i.e. the number and
-- size of its dimentions.
data ShapeExpr =
    ShapeId Integer Name
  -- ^ Shape id stores the number of dimentions which we should always know at
  -- compile time
  | ShapeConst [Integer]
  | ShapeSum [ShapeExpr]
  deriving(Show,Read,Ord,Eq)

shapeNDim :: ShapeExpr -> Integer
shapeNDim (ShapeId x _) = x
shapeNDim (ShapeConst x) = toInteger $ length x

-- | Scalar expressions
data Expr =
    EConst Const
  -- ^ Plain constant
  | ECall Name [Expr]
  -- ^ Call of a function or an operator
  | ESlice TenExpr [DimExpr]
  -- ^ Accessing an individual element of a tensor
  deriving(Show,Read,Ord,Eq)

instance Num Expr where
  (+) a b = ECall (Name "+") [a,b]
  (-) a b = ECall (Name "-") [a,b]
  (*) a b = ECall (Name "*") [a,b]
  negate a = ECall (Name "-") [a]
  abs = error "abs is undefined"
  signum = error "signum is undefined"
  fromInteger = EConst . CInt

data Type =
    TypeFloat32
  | TypeInt32
  | Tensor Type ShapeExpr
  deriving(Show,Read,Ord,Eq)

float32 = TypeFloat32

-- | Common arguments to various functions
data Args = Args {
    a_name :: Maybe Name
  , a_shape :: Maybe ShapeExpr
  , a_type :: Maybe Type
  } deriving(Show,Read,Ord,Eq)

nullArgs :: Args
nullArgs = Args mempty Nothing (Just float32)

-- | Pattern is a name of Tensor Expression
data Pattern = Pattern {
    p_name :: Name
  } deriving(Show,Read,Ord,Eq)

-- | Tensor Expressions. Allow us to write code like
-- `Tensor a,b; Tensor c = a + b;`
data TenExpr =
    TenPlh Placeholder
  -- ^ Placeholder is a tensor, which are to be supplied in runtime
  | TenId Pattern
  -- ^ Pattern is the target of assignment
  | TenLet Pattern TenExpr TenExpr
  | TenTuple [TenExpr]
  | TenDim DimExpr
  | TenShape ShapeExpr
  | TenCompute ShapeExpr Pattern Expr
  | TenDef Name TenExpr
  -- ^ FIXME: TenDef would be redundant in the presence of typechecker.
  | TenCall { tc_fname::Name, tc_attrs::Args, tc_args::[TenExpr] }
  -- ^ Function call. `tc_fname` is the name of a function. `tc_attrs` is
  -- common non-Tensor arguments to this function. `tc_args` is the Tensor
  -- arguments.
  deriving(Show,Read,Ord,Eq)


type Placeholder = (Name,Type,ShapeExpr)

pls_name :: Placeholder -> Name
pls_name (nm,_,_) = nm

newtype Function = Function { unFunction :: TenExpr }
  deriving(Read,Show,Eq,Ord)

newtype Library = Library { unLibrary :: TenExpr }
  deriving(Read,Show,Eq,Ord)





