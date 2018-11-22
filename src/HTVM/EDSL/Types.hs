{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HTVM.EDSL.Types where

import Data.Monoid
import Data.Text(Text)

-- | Name is the string convertable to valid C/C++ identifier
newtype Name = Name { n_get :: Text }
  deriving(Show,Read,Ord,Eq,Semigroup,Monoid)

data Const =
    CInt Integer
  | CFloat32 Float
  deriving(Show,Read,Ord,Eq)

-- | Dimention expression represents the length of the vectors, number of
-- rows/columns in a matrix, etc. Converts to `tvm::Var`.
data DimExpr =
    DimConst Integer
  | DimId Name
  | DimCall Name [DimExpr]
  | DimCtr Text              -- ^ Dim variable constructor `tvm::var`
  deriving(Show,Read,Ord,Eq)

instance Num DimExpr where
  (+) a b = DimCall (Name "+") [a,b]
  (-) a b = DimCall (Name "-") [a,b]
  (*) a b = DimCall (Name "*") [a,b]
  negate a = DimCall (Name "-") [a]
  abs = error "abs is undefined for DimExpr"
  signum = error "signum is undefined for DimExpr"
  fromInteger = DimConst

-- | Axis represents iterator running through the range supplied. Equivalent of
-- `tvm::IterVar`.
data Axis = Axis Name (DimExpr,DimExpr)
  deriving(Show,Read,Ord,Eq)

-- | Shape expressions represents the shape of a tensor, i.e. the number and
-- size of its dimentions. Rough equivalent of `tvm::Array<Expr>`.
data ShapeExpr =
    ShapeId Integer Name     -- ^ Shape id stores the number of dimentions which we should always know at
                             --   compile time
  | ShapeVector DimExpr      -- ^ Vector has 1 dimention of some length.
  | ShapeScalar              -- ^ Scalar has 0 dimentions.
  | ShapeSum ShapeExpr ShapeExpr
                             -- ^ Concatenation on shapes
  deriving(Show,Read,Ord,Eq)

-- | Return the number of dimentions of ShapeExpr which is always known at compile time.
shapeDim :: ShapeExpr -> Integer
shapeDim (ShapeId ndim _) = ndim
shapeDim (ShapeVector _) = 1
shapeDim (ShapeScalar) = 0
shapeDim (ShapeSum se1 se2) = shapeDim se1 + shapeDim se2

instance Semigroup ShapeExpr where
  (<>) a b = ShapeSum a b

shape :: [DimExpr] -> ShapeExpr
shape des = undefined

-- | Convert ShapeExpr in flattern form, where each list itme represents a
-- dimention, either of known size or unknown at compile time. Empty list
-- represents a shape of scalar.
-- FIXME: This function is impossible
-- shapeFlattern :: ShapeExpr -> [Either DimExpr Integer]
-- shapeFlattern sh =
--   case sh of
--     ShapeId 1 n -> [Left n]
--     ShapeId x n -> error "shapeFlattern: don't know how to represent multidimentional shape variables"
--     ShapeVector x -> [Right x]
--     ShapeScalar -> []
--     ShapeSum a b -> shapeFlattern a <> shapeFlattern b

-- | Scalar expressions
data Expr =
    EConst Const             -- ^ A constant
  | EId Name                 -- ^ A variable
  | EShapeSlice ShapeExpr Integer
                             -- ^ Access a certain dimention of ShapeExpr
  | ETenSlice TenExpr [Expr] -- ^ Accessing an individual element of a tensor
  | ECall Name [Expr]        -- ^ Call of a function or an operator
  | ETuple [Expr]            -- ^ A tuple of expressions
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

-- | Pattern is a left-hand-side of assignment Expression
data Pattern =
    PTensor Name  -- ^ Tensor
  | PShape Name   -- ^ Array<Expr>
  | PVar Name     -- ^ Var
  | PFunc Name    -- ^ LoweredFunc
  | PAxis Name    -- ^ Array<Var>
  | PTenTuple Name
  | PFuncTuple Name
  deriving(Show,Read,Ord,Eq)

-- | Tensor Expressions. Allow us to write code like
-- `Tensor a,b; Tensor c = a + b;`
data TenExpr =
    TenPlh Placeholder                  -- ^ Placeholder is a tensor, which are
                                        --   to be supplied in runtime
  | TenId Name
  | TenLet Pattern TenExpr TenExpr
  | TenTuple [TenExpr]
  | TenDim DimExpr
  | TenShape ShapeExpr
  | TenAxis Axis
  | TenCompute ShapeExpr Pattern Expr
  | TenDef Text TenExpr                 -- ^ Name and Expression of function
                                        --   definition.
                                        --   FIXME: TenDef would be redundant
                                        --   in the presence of typechecker.
  | TenCall { tc_fname::Name, tc_attrs::Args, tc_args::[TenExpr] }
                                        -- ^ Function call. `tc_fname` is the
                                        --   name of a function. `tc_attrs` is
                                        --   common non-Tensor arguments to this
                                        --   function. `tc_args` is the Tensor
                                        --   arguments.
  deriving(Show,Read,Ord,Eq)


type Placeholder = (Text,Type,ShapeExpr)

-- pls_name :: Placeholder -> Name
-- pls_name (nm,_,_) = nm

newtype Function = Function { unFunction :: TenExpr }
  deriving(Read,Show,Eq,Ord)

data Module = Module { modFuncs :: [Function] , modExpr :: TenExpr }
  deriving(Read,Show,Eq,Ord)

data ModuleGenSrc = ModuleGenSrc Module Text
  deriving(Show,Read,Eq,Ord)

data ProgramSrc = ProgramSrc Text
  deriving(Show,Read,Eq,Ord)

data ProgramBin = ProgramBin FilePath
  deriving(Show,Read,Eq,Ord)

data ModuleGen = ModuleGen FilePath Module
  deriving(Show,Read,Eq,Ord)

data Assembly = Assembly Module String
  deriving(Show,Read,Eq,Ord)

data ModuleLib = ModuleLib FilePath Module
  deriving(Show,Read,Eq,Ord)




