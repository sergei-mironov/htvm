{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HTVM.EDSL.Monad where

import qualified Data.Text as Text

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Maybe (fromMaybe,fromJust)
import Data.Text (Text)

import HTVM.Prelude
import HTVM.EDSL.Types

class TensorLike a where
  getTenExpr :: a -> TenExpr
  modifyTenExpr :: a -> TenExpr -> a
  toPattern :: Name -> Pattern

data ExprCtx = ExprCtx {
   ec_expr :: Maybe Expr
  } deriving(Show)

initExprCtx = ExprCtx Nothing

newtype ExprT m a = ExprT { unExprT :: StateT ExprCtx m a }
  deriving(Functor,Applicative,Monad,MonadTrans)

runExprT :: (Monad m) => ExprT m Expr -> m (Expr,ExprCtx)
runExprT e = flip runStateT initExprCtx $ unExprT e

stageExpr :: (Monad m) => ExprT m Expr -> m Expr
stageExpr e = fst <$> runExprT e

-- | Monadic context of the tensor expression builder monad
data StmtCtx = StmtCtx {
    sc_gen :: Integer
  -- ^ Name generator counter
  , sc_expr :: TenExpr -> TenExpr
  -- ^ Expression which is being build
  -- TODO: Implement dictionary containing schedulings for tensors
  }

-- | Initial context for tensor expression builder monad
initStmtCtx = StmtCtx 0 id

-- | Monad transformer for building `TenExpr`.
-- FIXME: Name `StmtT` is somewhat misleading.
newtype StmtT m a = StmtT { unStmtT :: StateT StmtCtx m a }
  deriving(Functor,Applicative,Monad,MonadTrans,MonadState StmtCtx,MonadIO)

type Stmt a = StmtT Identity a

name :: (Monad m) => Text -> m Name
name = return . Name

-- | Produce unique name in the current context, optionally preffixed or
-- suffixed
fresh' :: (Monad m) => Text -> Text -> StmtT m Name
fresh' pref suff = StmtT $ state $ \s@StmtCtx{..} -> (Name $ wrap pref <> tshow sc_gen <> wrap suff, s{sc_gen = sc_gen+1})
  where
    wrap x = if x == "" then x else x <> "_"

-- | Generate new preffixed and suffixed unique name
freshP,freshS :: (Monad m) => Text -> StmtT m Name
freshP p = fresh' p ""
freshS s = fresh' "" s

-- | Produce unique name in the current context
fresh :: (Monad m) => StmtT m Name
fresh = fresh' "" ""

runStmtT :: (Monad m) => StmtCtx -> StmtT m a -> m (a,StmtCtx)
runStmtT ctx s = flip runStateT ctx $ unStmtT s

stageTenExpr :: (Monad m) => StmtT m TenExpr -> m TenExpr
stageTenExpr s = stage <$> runStmtT initStmtCtx s where
  stage (te,StmtCtx{sc_expr}) = sc_expr te

-- | Function represents TVM expression which is a valid `Module`-function definition
-- Note that Module-functions ate not first-class objects in TVM (TODO: check
-- that fact).
-- TODO: Isn't it too complex? Should we replace it with 1-to-1 LoweredFunc wrapper?
data LoweredFunc = LoweredFunc {
    lfuncName :: Text
  -- ^ Function name
  , lfuncDefExpr :: TenExpr
  -- ^ Defenition expression, as seen by `lower` function
  , lfuncRefExpr :: TenExpr
  -- ^ Reference expression, may be either full definition or reference
  }
  deriving(Read,Show,Eq,Ord)

instance TensorLike LoweredFunc where
  getTenExpr = lfuncRefExpr
  modifyTenExpr lf te = lf{lfuncRefExpr = te}
  toPattern = PLoweredFunc

-- | Returned module contains all its definitions.
stageStmtT :: (Monad m, TensorLike t) => StmtT m t -> m t
stageStmtT s = stage <$> runStmtT initStmtCtx s where
  stage (t,StmtCtx{sc_expr}) = modifyTenExpr t (sc_expr (getTenExpr t))

stageStmt :: (TensorLike t) => StmtT Identity t -> t
stageStmt = runIdentity . stageStmtT

data Tensor = Tensor TenExpr
  deriving(Show,Read,Eq,Ord)

assign_ :: (Monad m) => Pattern -> TenExpr -> StmtT m ()
assign_ p te1 = do
  modify $ \s -> s{sc_expr = \te -> (sc_expr s) (TenLet p te1 te)}

assignN :: (Monad m) => (Name -> Pattern) -> Text -> TenExpr -> StmtT m TenExpr
assignN mkpat prefix te1 = do
  n <- freshP prefix
  assign_ (mkpat n) te1
  return (TenId n)

instance TensorLike Tensor where getTenExpr (Tensor te) = te; modifyTenExpr = const Tensor; toPattern = PTensor

assign :: forall m t . (TensorLike t, Monad m) => t -> StmtT m t
assign t = modifyTenExpr t <$> assignN (toPattern @t) "val" (getTenExpr t)

-- FIXME: return from placeholders
data Plh = Plh TenExpr
  deriving(Read,Show,Eq,Ord)

placeholder :: Text -> Type -> ShapeExpr -> Tensor
placeholder nm tp shp = Tensor $ TenPlh (nm,tp,shp)

-- | Define a module function. Accepts its name @n@, Placeholder definitions
-- @plh@ which become a type of arguments and a lambda function @fbody@ defining
-- the body.  List passed to @fbody@ would have same length as @plh@.
lfunction :: (Monad m) => Text -> [Placeholder] -> ([Tensor] -> StmtT m Tensor) -> StmtT m LoweredFunc
lfunction nam plhs fbody = do
  ts <- mapM (\(n,t,s) -> assign $ placeholder n t s) plhs
  res <- fbody ts
  lower nam (schedule [res]) (ts<>[res])

-- | Lowered module
data LModule = LModule { lmodFuncNames :: [Text], lmodExpr :: TenExpr }
  deriving(Read,Show,Eq,Ord)

instance TensorLike LModule where
  getTenExpr = lmodExpr
  modifyTenExpr lm e = lm { lmodExpr = e }
  toPattern = PLModule

lower :: (Monad m) => Text -> Schedule -> [Tensor] -> StmtT m LoweredFunc
lower fname (Schedule s) plh =
  let
    f = TenSlice (TenCall $ TenAPI_Lower fname s [t|Tensor t<-plh]) 0
  in do
  StmtCtx{..} <- get
  assign $ LoweredFunc fname (sc_expr f) f

data Tuple = Tuple TenExpr
  deriving(Show,Read,Eq,Ord)

instance TensorLike Tuple where getTenExpr (Tuple te) = te; modifyTenExpr = const Tuple; toPattern = PTenTuple

batchCompute' :: ShapeExpr -> Name -> (Expr -> [Expr]) -> TenExpr
batchCompute' se nm body = TenCompute se (PAxis nm) (ETuple $ body (EId nm))

batchCompute :: (Monad m) => ShapeExpr -> (Expr -> [Expr]) -> StmtT m Tuple
batchCompute se tbody = do
  axis <- freshP "bcomp"
  assign (Tuple $ batchCompute' se axis tbody)

uniCompute :: (Monad m) => ShapeExpr -> (Expr -> Expr) -> StmtT m Tensor
uniCompute se ebody = do
  axis <- freshP "comp"
  assign (Tensor $ flip TenSlice 0 $ batchCompute' se axis ((\x -> [x]) . ebody))

-- | Specialize computes to different number of dimentsions
class Computable a where
  compute :: (Monad m) => ShapeExpr -> (a -> Expr) -> StmtT m Tensor

-- TODO: assert the number of dimentions in @se@ equals to number of elements in axis tuple
instance Computable (Expr) where compute se f = uniCompute se (\e -> f (e!0))
instance Computable (Expr,Expr) where compute se f = uniCompute se (\e -> f (e!0,e!1))
instance Computable (Expr,Expr,Expr) where compute se f = uniCompute se (\e -> f (e!0,e!1,e!2))
instance Computable (Expr,Expr,Expr,Expr) where compute se f = uniCompute se (\e -> f (e!0,e!1,e!2,e!3))
instance Computable (Expr,Expr,Expr,Expr,Expr) where compute se f = uniCompute se (\e -> f (e!0,e!1,e!2,e!3,e!4))

-- | Version of assign where the computation rule is specified for each
-- Tensor's item
-- compute :: (Monad m) => ShapeExpr -> ([Expr] -> Expr) -> StmtT m Tensor
-- compute se ebody =
--   let
--     dims = [0..(shapeDim se)-1]
--   in do
--   axis <- freshP "vars"
--   assign $ compute' (TenShape se) axis (\x -> ebody (map (EShapeSlice x) dims))

-- | Call a function
-- call :: TenFuncName -> [TenExpr] -> TenExpr
-- call fname args = TenCall fname (map TenArg args)

ecall :: ExprFuncName -> [Expr] -> Expr
ecall nm args = ECall nm args

dimvar :: (Monad m) => StmtT m DimExpr
dimvar = do
  nm <- freshP "var"
  assign_ (PVar nm) (TenDim (DimCtr $ n_get nm))
  return (DimId nm)

shp :: [DimExpr] -> ShapeExpr
shp de = foldr1 ShapeSum (map ShapeVector de)

shapevar :: (Monad m) => [DimExpr] -> StmtT m ShapeExpr
shapevar de = do
  n <- assignN PShape "shape" (TenShape (shp de))
  return (ShapeTen n)

-- | FIXME: Module returned is only valid in the context of StmtT monad's state.
-- One should encode this fact in types
lmodul :: (Monad m) => [LoweredFunc] -> StmtT m LModule
lmodul lfns = do
  n <- assignN PFuncTuple "lmod" (TenTuple (map lfuncRefExpr lfns))
  return $ LModule (map lfuncName lfns) n

-- | FIXME: Convertion from TenExpr to Expr looks weitd. Rethink returning
-- expression from statement monad
axisId :: (Monad m) => Tensor -> Integer -> StmtT m IterVar
axisId (Tensor t) i = IterVar . (\(TenId n) -> EId n) <$> assignN PIterVar "axis"
  (TenCall $ TenAPI_AxisId t i)

data IterVar = IterVar Expr
  deriving(Show,Read,Eq,Ord)

-- | FIXME: Convertion from TenExpr to Expr looks weitd. Rethink returning
-- expression from statement monad
reduce_axis :: (Monad m) => (DimExpr,DimExpr) -> StmtT m IterVar
reduce_axis (a,b) = IterVar . (\(TenId n) -> EId n) <$> assignN PIterVar "reduce_axis"
  (TenCall $ TenAPI_ReduceAxis $ TenTuple [TenDim a, TenDim b])

infixr 8 !

class Sliceable a b c | a->b, a->c where
  (!) :: a -> b -> c

instance Sliceable Tensor [Expr] Expr where
  (!) :: Tensor -> [Expr] -> Expr
  (!) (Tensor t) sl = ETenSlice t sl

instance Sliceable Tuple Integer Tensor where
  (!) :: Tuple -> Integer -> Tensor
  (!) (Tuple t) sl = Tensor $ TenSlice t sl

instance Sliceable ShapeExpr Integer Expr where
  (!) :: ShapeExpr -> Integer -> Expr
  (!) t sl = EShapeSlice t sl

instance Sliceable Expr Integer Expr where
  (!) :: Expr -> Integer -> Expr
  (!) e i = ESlice e i


{-
 _____ ___  ____ ___   ____  _           _ _
|_   _/ _ \|  _ \_ _| | __ )(_)_ __   __| (_)_ __   __ _ ___
  | || | | | |_) | |  |  _ \| | '_ \ / _` | | '_ \ / _` / __|
  | || |_| |  __/| |  | |_) | | | | | (_| | | | | | (_| \__ \
  |_| \___/|_|  |___| |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
                                                   |___/

FIXME: Bindings are highly C++ - specific, rethink
-}

op1 nam (Tensor a) = Tensor $ TenCall $ TenAPI_Op nam [a]
op2 nam (Tensor a) (Tensor b) = Tensor $ TenCall $ TenAPI_Op nam [a,b]

elemwise1 op (Tensor a) = Tensor $ TenCall $ TenAPI_Elemwise op [a]
elemwise2 op (Tensor a) (Tensor b) = Tensor $ TenCall $ TenAPI_Elemwise op [a,b]

instance Num Tensor where
  (+) = op2 "+"
  (-) = op2 "-"
  (*) = op2 "*"
  negate = op1 "-"
  abs = elemwise1 "abs"
  signum = elemwise1 "sign"
  fromInteger = error "fromInteger is not implemented for Tensor"

instance Fractional Tensor where
  fromRational = error "fromRational is not implemented for Tensor"
  (/) = op2 "/"

instance Floating Tensor where
  pi = error "pi is not defined for Tensor" {- we should know shape to actually define pi -}
  exp = elemwise1 "exp"
  log = elemwise1 "log"
  sqrt = elemwise1 "sqrt"
  (**) = elemwise2 "pow"
  logBase = elemwise2 "logBase"
  sin = elemwise1 "sin"
  cos = elemwise1 "cos"
  tan = elemwise1 "tan"
  asin = elemwise1 "asin"
  acos = elemwise1 "acos"
  atan = elemwise1 "atan"
  sinh = elemwise1 "sinh"
  cosh = elemwise1 "cosh"
  tanh = elemwise1 "tanh"
  asinh = elemwise1 "asinh"
  acosh = elemwise1 "acosh"
  atanh = elemwise1 "atanh"

esum :: (Expr,[Expr]) -> Expr
esum (a,rs) = ecall ExprSum [a, ETuple rs]

-- data Conv2dArgs = Conv2dArgs {
--     conv2d_stride :: (Integer,Integer)
--   , conv2d_padding ::(Integer,Integer)
--   , conv2d_dilation :: (Integer,Integer)
--   , conv2d_type :: Type
--   , conv2d_name :: Text
--   } deriving(Read,Show,Eq,Ord)

-- instance HasDefault Conv2dArgs where
--   def = Conv2dArgs (1,1) (1,1) (1,1) TypeFloat32 "conv2d"

conv2d_nchw :: Tensor -> Tensor -> TenAPI_Conv2dArgs -> Tensor
conv2d_nchw (Tensor x) (Tensor k) ca = Tensor $ TenCall $ TenAPI_Conv2d x k ca

-- data PadArgs = PadArgs {
--     pad_before :: [Expr]
--   , pad_after :: [Expr]
--   , pad_value :: Expr
--   , pad_name :: Text
--   } deriving(Read,Show,Eq,Ord)

-- instance HasDefault PadArgs where
--   def = PadArgs [] [] 0 "pad"

pad :: Tensor -> TenAPI_PadArgs -> Tensor
pad (Tensor x) pa = Tensor $ TenCall $ TenAPI_Pad x pa

matmul :: Tensor -> Tensor -> Tensor
matmul (Tensor a) (Tensor b) = Tensor $ TenCall $ TenAPI_MatMul a b

dense :: Tensor -> Tensor -> Tensor -> Tensor
dense (Tensor x) (Tensor w) (Tensor b) = Tensor $ TenCall $ TenAPI_Dense x w b

broadcast_to :: Tensor -> ShapeExpr -> Tensor
broadcast_to (Tensor a) se = Tensor $ TenCall $ TenAPI_BroadcastTo a se

sigmoid :: Tensor -> Tensor
sigmoid = elemwise1 "sigmoid"

relu :: Tensor -> Tensor
relu = elemwise1 "relu"

flatten :: Tensor -> Tensor
flatten (Tensor a) = Tensor $ TenCall $ TenAPI_Flatten a

split :: Tensor -> [Integer] -> Integer -> Tuple
split (Tensor a) indices axis = Tuple $ TenCall $ TenAPI_Split a indices axis


differentiate :: Tensor -> [Tensor] -> Tuple
differentiate (Tensor a) ts = Tuple $ TenCall $ TenAPI_Differentiate a (TenTuple [t|(Tensor t)<-ts])

{-
 ____       _              _       _
/ ___|  ___| |__   ___  __| |_   _| | ___
\___ \ / __| '_ \ / _ \/ _` | | | | |/ _ \
 ___) | (__| | | |  __/ (_| | |_| | |  __/
|____/ \___|_| |_|\___|\__,_|\__,_|_|\___|

 ____  _           _ _
| __ )(_)_ __   __| (_)_ __   __ _ ___
|  _ \| | '_ \ / _` | | '_ \ / _` / __|
| |_) | | | | | (_| | | | | | (_| \__ \
|____/|_|_| |_|\__,_|_|_| |_|\__, |___/
                             |___/

-}

data Schedule = Schedule TenExpr
  deriving(Show,Read,Eq,Ord)

instance TensorLike Schedule where getTenExpr (Schedule s) = s; modifyTenExpr = const Schedule; toPattern = PSchedule

schedule :: [Tensor] -> Schedule
schedule ts = Schedule $ TenCall $ TenAPI_Schedule [t|Tensor t<-ts]

parallel :: (Monad m) => Schedule -> Tensor -> IterVar -> StmtT m ()
parallel (Schedule s) (Tensor t) (IterVar a) =
  return () <* assignN PStage "stage" (TenCall $ TenAPI_Parallel s t (TenExpr a))

