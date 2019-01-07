{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module HTVM.EDSL.Types where

import Data.Monoid
import Data.Text(Text)

-- | Name represents valid C/C++ identifier
newtype Name = Name { n_get :: Text }
  deriving(Show,Read,Ord,Eq,Semigroup,Monoid)

-- | Const encodes valid C/C++ constants
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

instance Semigroup ShapeExpr where
  (<>) a b = ShapeSum a b

-- | A registry of expression-level function names
data ExprFuncName =
    ExprOp Text
  | ExprSum
  | ESigmoid
  deriving(Show,Read,Ord,Eq)

-- | Scalar expressions, equivalent of `tvm::Expr`
data Expr =
    EConst Const             -- ^ A constant
  | EId Name                 -- ^ A variable
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

-- | Representation of tvm type codes
data Type =
    TypeFloat32
  | TypeInt32
  | TypeTensor Type ShapeExpr
  deriving(Show,Read,Ord,Eq)

float32 :: Type
float32 = TypeFloat32

-- | Pattern represents left-hand-side of C/C++ assignments
--
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

-- | A registry of tensor-level function names
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
  | TenBroadcastTo
  | TenFlatten
  | TenDense
  deriving(Show,Read,Ord,Eq)

-- | Kinds of arguments received by `TenCall`
data TenArg =
    TenArg TenExpr           -- ^ Ordinary argument, another `TenExpr`
  | StrArg Text              -- ^ String argument
  | IntArg Integer           -- ^ Integer argument TODO: remove?
  | IntsArg [Integer]        -- ^ Integer argument TODO: remove?
  | TypeArg Type             -- ^ Type argument
  | LayoutArg Layout         -- ^ Layout argument
  | ShapeArg ShapeExpr
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
    TenId Name               -- ^ Identifier
  | TenPlh Placeholder
                             -- ^ Placeholder is a disting kind of TenExpr because it
                             --   refers `Type` and `ShapeExpr` which are not `TenExpr`
                             --
                             --   FIXME: Replace `TenPlh` and `TenDef` with a
                             --   function representation TenFun
  | TenTuple [TenExpr]
  | TenSlice TenExpr Integer -- ^ Slice `TenTuple`
  | TenDim DimExpr           -- ^ Dimention expression
  | TenShape ShapeExpr       -- ^ Shape expression
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


-- | Placeholder collects information about entry or exit points of TVM programs
type Placeholder = (Text,Type,ShapeExpr)


-- | ModuleGenSrc represents C++ sources of Module generator. mgen_mod is a
-- user-defined data representing some initial AST of the module
data ModuleGenSrc a = ModuleGenSrc { mgen_mod :: a, mgen_src :: Text }
  deriving(Show,Read,Eq,Ord)

-- | Represents C++ sources arbitrary program
data ProgramSrc = ProgramSrc { prog_src :: Text }
  deriving(Show,Read,Eq,Ord)

-- | Represent path to arbitrary program's binary
data ProgramBin = ProgramBin FilePath
  deriving(Show,Read,Eq,Ord)

-- | Represent path to Module generator binary
data ModuleGen a = ModuleGen FilePath a
  deriving(Show,Read,Eq,Ord)

-- | LLVM Assembly produced by Module generator, along with source Module
data Assembly a = Assembly a String
  deriving(Show,Read,Eq,Ord)

-- | Path to compiled Module along with its source expression
data ModuleLib a = ModuleLib FilePath a
  deriving(Show,Read,Eq,Ord)

