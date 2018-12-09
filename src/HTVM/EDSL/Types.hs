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
  | DimCtr Text              -- ^ Dim constructor aka `tvm::var`
  deriving(Show,Read,Ord,Eq)

instance Num DimExpr where
  (+) a b = DimCall (Name "+") [a,b]
  (-) a b = DimCall (Name "-") [a,b]
  (*) a b = DimCall (Name "*") [a,b]
  negate a = DimCall (Name "-") [a]
  abs = error "abs is undefined for DimExpr"
  signum = error "signum is undefined for DimExpr"
  fromInteger = DimConst

-- -- | Axis represents iterator running through the range supplied. Equivalent of
-- -- `tvm::IterVar`.
-- data Axis = Axis Name (DimExpr,DimExpr)
--   deriving(Show,Read,Ord,Eq)

-- | Shape expressions represents the shape of a tensor, i.e. the number and
-- size of its dimentions. Rough equivalent of `tvm::Array<Expr>`.
data ShapeExpr =
    ShapeTen TenExpr
                             -- ^ Shape extractor. Only valid for `TenPlh` ,
                             -- `TenCompute` and `TenShape` itself.
  | ShapeVector DimExpr      -- ^ Vector has 1 dimention of some length.
  | ShapeScalar              -- ^ Scalar has 0 dimentions.
  | ShapeSum ShapeExpr ShapeExpr
                             -- ^ Concatenation on shapes
  deriving(Show,Read,Ord,Eq)

-- | Return the number of dimentions of ShapeExpr which is always known at compile time.
-- TODO: Move to `Eval.hs` as a generic algorithm
-- shapeDim :: ShapeExpr -> Integer
-- shapeDim (ShapeTen ndim _) = ndim
-- shapeDim (ShapeVector _) = 1
-- shapeDim (ShapeScalar) = 0
-- shapeDim (ShapeSum se1 se2) = shapeDim se1 + shapeDim se2

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

data ExprFuncName =
    ExprOp Text
  | ExprSum
  | ESigmoid
  deriving(Show,Read,Ord,Eq)

-- | Scalar expressions
data Expr =
    EConst Const             -- ^ A constant
  | EId Name                 -- ^ A variable
  -- | EShape ShapeExpr         -- ^ A shape expression
  | EShapeSlice ShapeExpr Integer
                             -- ^ Access a certain dimention of ShapeExpr
  | ETenSlice TenExpr [Expr] -- ^ Accessing an individual element of a tensor
  | ECall ExprFuncName [Expr]-- ^ Call of a function or an operator
  | ESlice Expr Integer      -- ^ Tuple slicing
  | ETuple [Expr]            -- ^ A tuple of expressions
  deriving(Show,Read,Ord,Eq)

instance Num Expr where
  (+) a b = ECall (ExprOp "+") [a,b]
  (-) a b = ECall (ExprOp "-") [a,b]
  (*) a b = ECall (ExprOp "*") [a,b]
  negate a = ECall (ExprOp "-") [a]
  abs = error "abs is undefined"
  signum = error "signum is undefined"
  fromInteger = EConst . CInt

data Type =
    TypeFloat32
  | TypeInt32
  | TypeTensor Type ShapeExpr
  deriving(Show,Read,Ord,Eq)

float32 = TypeFloat32

-- | Common arguments to various functions
data Args = Args {
    a_name :: Maybe Name
  , a_shape :: Maybe ShapeExpr
  , a_type :: Maybe Type
  } deriving(Show,Read,Ord,Eq)

nullArgs :: Args
nullArgs = Args Nothing Nothing Nothing

-- | Pattern is a left-hand-side of assignments
-- FIXME: Separate type codes from Name binding
data Pattern =
    PTensor Name             -- ^ Tensor
  | PShape Name              -- ^ Array<Expr>
  | PVar Name                -- ^ Var
  | PIterVar Name            -- ^ IterVar
  | PFunc Name               -- ^ LoweredFunc
  | PAxis Name
  | PTenTuple Name
  | PFuncTuple Name
  | PSchedule Name
  | PStage Name
  deriving(Show,Read,Ord,Eq)

-- | List of valid Tensor-Expression level function names
data TenFuncName =
    TenOp Text
  | TenReduceAxis
  | TenConv2d_NCHW
  | TenPad
  | TenSchedule
  | TenParallel
  | TenAxisId
  | TenMatMul
  | TenElemwise Text
  | TenSplit
  | TenDifferentiate
  deriving(Show,Read,Ord,Eq)

-- | `TenCall` receive arguments of the following kinds
data TenArg =
    TenArg TenExpr           -- ^ Ordinary argument, another `TenExpr`
  | StrArg Text              -- ^ String argument
  | IntArg Integer           -- ^ Integer argument TODO: remove?
  | IntsArg [Integer]        -- ^ Integer argument TODO: remove?
  | TypeArg Type             -- ^ Type argument
  | LayoutArg Layout         -- ^ Layout argument
  deriving(Show,Read,Ord,Eq)

-- | Convolution layout
data Layout = NCHW | NWCN | NHWC
  deriving(Show,Read,Ord,Eq)

-- | Tensor Expressions. Allow us to write code like
-- `Tensor a,b; Tensor c = a + b;`
--
-- Notes:
--   * We don't keep Type as a part of TenExpr since in theory we shouldn't need
--     it (assuming the typechecker is present)
data TenExpr =
    TenId Name
  | TenPlh Placeholder
                             -- ^ Placeholder is a disting kind of TenExpr because it
                             --   refers `Type` and `ShapeExpr` which are not `TenExpr`
  | TenTuple [TenExpr]
  | TenSlice TenExpr Integer -- ^ Slice `TenTuple`
  | TenDim DimExpr
  | TenShape ShapeExpr
  | TenExpr Expr
                             -- ^ We need TenExpr to encode `reduce_axis` results. It returns
                             --   sliceable expressions
  | TenLet Pattern TenExpr TenExpr
  | TenCompute ShapeExpr Pattern Expr
  | TenDef Text TenExpr
                             -- ^ Name and Expression of function definition.
                             --   FIXME: TenDef would be redundant in the presence of
                             --   typechecker.
  | TenCall { tc_fname::TenFuncName, tc_args::[TenArg] }
                             -- ^ Function call.
                             --   `tc_fname` is the name of a function.
                             --   `tc_args` is its arguments.
  deriving(Show,Read,Ord,Eq)


type Placeholder = (Text,Type,ShapeExpr)

-- pls_name :: Placeholder -> Name
-- pls_name (nm,_,_) = nm

-- data Axis = Axis {aExpr :: TenExpr}
--   deriving(Read,Show,Eq,Ord)




