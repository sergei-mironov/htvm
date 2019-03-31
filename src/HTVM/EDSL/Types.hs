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
  | PLoweredFunc Name
  | PLModule Name
  deriving(Show,Read,Ord,Eq)

-- | A registry of tensor-level function names
-- FIXME: replace with direct API encoding
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
  | TenLower
  deriving(Show,Read,Ord,Eq)

class HasDefault a where
  def :: a

data TenAPI_Conv2dArgs = TenAPI_Conv2dArgs {
    conv2d_stride :: (DimExpr,DimExpr) -- ^ FIXME: replace with Expr?  (here and below)
  , conv2d_padding ::(DimExpr,DimExpr)
  , conv2d_dilation :: (DimExpr,DimExpr)
  , conv2d_type :: Type
  , conv2d_name :: Text
  , conv2d_layout :: Layout
  } deriving(Read,Show,Eq,Ord)

instance HasDefault TenAPI_Conv2dArgs where
  def = TenAPI_Conv2dArgs (1,1) (1,1) (1,1) TypeFloat32 "conv2d" NCHW

data TenAPI_PadArgs = TenAPI_PadArgs {
    pad_before :: [Expr]
  , pad_after :: [Expr]
  , pad_value :: Expr
  , pad_name :: Text
  } deriving(Read,Show,Eq,Ord)

instance HasDefault TenAPI_PadArgs where
  def = TenAPI_PadArgs [] [] 0 "pad"

data TenAPI =
    TenAPI_Op Text {-^ Operation name -} [TenExpr] {-^ operands unary,binary,etc -}
  | TenAPI_ReduceAxis TenExpr
  | TenAPI_Conv2d TenExpr {- ^ input -} TenExpr {-^ kernel -} TenAPI_Conv2dArgs
  | TenAPI_Pad TenExpr {-^ input -} TenAPI_PadArgs
  | TenAPI_Schedule [TenExpr]
  | TenAPI_Parallel TenExpr {- ^ schedule -} TenExpr {- ^ inp -} TenExpr {- ^ IterVar -}
  | TenAPI_AxisId TenExpr {- ^ tensor -} Integer {- ^ Axis ID -}
  | TenAPI_MatMul TenExpr TenExpr
  | TenAPI_Elemwise Text {- ^ Op name -} [TenExpr] {-^ Operands -}
  | TenAPI_Split TenExpr [Integer] Integer
  | TenAPI_Differentiate TenExpr TenExpr -- FIXME: support list of targets
  | TenAPI_BroadcastTo TenExpr {- ^ What -} ShapeExpr {- ^ To which shape -}
  | TenAPI_Flatten TenExpr
  | TenAPI_Dense TenExpr {-^ c -} TenExpr {-^ w -} TenExpr {- ^ b -}
  | TenAPI_Lower Text {-^ fname -} TenExpr {-^ Schedule -} [TenExpr] {-^ placeholders -}
  deriving(Show,Read,Ord,Eq)

-- | Kinds of arguments received by `TenCall`
-- FIXME deprecated
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
  | TenCall TenAPI           -- ^ API function call.
  deriving(Show,Read,Ord,Eq)


-- | Placeholder collects information about entry or exit points of TVM programs
type Placeholder = (Text,Type,ShapeExpr)


-- | ModuleGenSrc represents C++ sources of Module generator. mgen_mod is a
-- user-defined data representing some initial AST of the module
data ModuleGenSrc a = ModuleGenSrc { mgen_mod :: a, mgen_src :: Text }
  deriving(Show,Read,Eq,Ord)

-- | Represents C++ sources of arbitrary program
data ProgramSrc = ProgramSrc { prog_src :: Text }
  deriving(Show,Read,Eq,Ord)

-- | Represent path to arbitrary program's binary
data ProgramBin = ProgramBin FilePath
  deriving(Show,Read,Eq,Ord)

-- | ModuleGen represents path to compiled TVM module generator binary
data ModuleGen a = ModuleGen FilePath a
  deriving(Show,Read,Eq,Ord)

-- | LLVM Assembly produced by TVM Module generator, along with source
-- expression
data Assembly a = Assembly a String
  deriving(Show,Read,Eq,Ord)

-- | Path to compiled TVM Module along with its source expression. Typically
-- the module is contained in a shared library
data ModuleLib a = ModuleLib FilePath a
  deriving(Show,Read,Eq,Ord)

