{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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


data StmtCtx = StmtCtx {
    sc_gen :: Integer
  , sc_expr :: TenExpr -> TenExpr
  }

initStmtCtx = StmtCtx 0 id

newtype StmtT m a = StmtT { unStmtT :: StateT StmtCtx m a }
  deriving(Functor,Applicative,Monad,MonadTrans,MonadState StmtCtx,MonadIO)

type Stmt a = StmtT Identity a

name :: (Monad m) => Text -> m Name
name = return . Name

fresh' :: (Monad m) => Text -> Text -> StmtT m Name
fresh' pref suff = StmtT $ state $ \s@StmtCtx{..} -> (Name $ wrap pref <> tshow sc_gen <> wrap suff, s{sc_gen = sc_gen+1})
  where
    wrap x = if x == "" then x else x <> "_"

-- | Generate new preffixed and suffixed names
freshP,freshS :: (Monad m) => Text -> StmtT m Name
freshP p = fresh' p ""
freshS s = fresh' "" s

fresh :: (Monad m) => StmtT m Name
fresh = fresh' "" ""

runStmtT :: (Monad m) => StmtCtx -> StmtT m a -> m (a,StmtCtx)
runStmtT ctx s = flip runStateT ctx $ unStmtT s

scope :: (Monad m) => StmtT m TenExpr -> StmtT m TenExpr
scope m = do
  ctx0 <- get
  (te,ctx1) <- lift $ runStmtT initStmtCtx{sc_gen=(sc_gen ctx0)} m
  put ctx0{sc_gen=sc_gen ctx1}
  -- traceM $ ppShow $ (sc_expr ctx1) te
  return $ (sc_expr ctx1) te

stageTenExpr :: (Monad m) => StmtT m TenExpr -> m TenExpr
stageTenExpr s = stage <$> runStmtT initStmtCtx s where
  stage (te,StmtCtx{..}) = sc_expr te

stageFunction :: (Monad m) => StmtT m Function -> m Function
stageFunction fe = Function <$> stageTenExpr (unFunction <$> fe)

-- | Returned module contains all its definitions.
-- FIXME: Encode self-contained Modules differently.
stageModuleT :: (Monad m) => StmtT m Module -> m Module
stageModuleT s = stage <$> runStmtT initStmtCtx s where
  stage (Module funcs te,StmtCtx{..}) = Module funcs (sc_expr te)

stageModule :: StmtT Identity Module -> Module
stageModule = runIdentity . stageModuleT

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

class TensorLike a where
  toTenExpr :: a -> TenExpr
  toPattern :: Name -> Pattern
  fromTenExpr :: TenExpr -> a

instance TensorLike Tensor where toTenExpr (Tensor te) = te; fromTenExpr = Tensor; toPattern = PTensor

assign :: forall m a . (TensorLike a, Monad m) => a -> StmtT m a
assign a = fromTenExpr <$> assignN (toPattern @a) "asgn" (toTenExpr a)

-- | Function represents TVM expression which is a valid `Module`-function definition
-- Note that Module-functions ate not first-class objects in TVM (TODO: check
-- that fact).
newtype Function = Function { unFunction :: TenExpr }
  deriving(Read,Show,Eq,Ord)

-- | Module contains a valid module expression and a set of module functions
data Module = Module { modFuncs :: [Function] , modExpr :: TenExpr }
  deriving(Read,Show,Eq,Ord)

-- | ModuleGenSrc is a C++ sources Module generator
data ModuleGenSrc = ModuleGenSrc { mgen_mod :: Module, mgen_src :: Text }
  deriving(Show,Read,Eq,Ord)

-- | Represents C++ sources arbitrary program
data ProgramSrc = ProgramSrc { prog_src :: Text }
  deriving(Show,Read,Eq,Ord)

-- | Represent path to arbitrary program's binary
data ProgramBin = ProgramBin FilePath
  deriving(Show,Read,Eq,Ord)

-- | Represent path to Module generator binary
data ModuleGen = ModuleGen FilePath Module
  deriving(Show,Read,Eq,Ord)

-- | LLVM Assembly produced by Module generator, along with source Module
data Assembly = Assembly Module String
  deriving(Show,Read,Eq,Ord)

-- | Path to compiled Module along with its source expression
data ModuleLib = ModuleLib FilePath Module
  deriving(Show,Read,Eq,Ord)

-- | Define a module function. Accepts its name @n@, Placeholder definitions
-- @plh@ which become a type of arguments and a lambda function @fbody@ defining
-- the body.  List passed to @fbody@ would have same length as @plh@.
function :: (Monad m) => Text -> [Placeholder] -> ([Tensor] -> StmtT m Tensor) -> StmtT m Function
function n plh fbody = do
  Function <$> do
    (\x -> assign_ (PFunc (Name n)) x >> pure (TenId (Name n))) =<< do
      scope $ do
        plhs <- forM plh $ assignN PTensor "plh" . TenPlh
        Tensor bres <- fbody (map Tensor plhs)
        res <- assignN PTenTuple "res" (TenTuple (plhs <> [bres]))
        modify $ \s -> s{sc_expr = \te -> TenDef n ((sc_expr s) te)}
        return res

data Tuple = Tuple TenExpr
  deriving(Show,Read,Eq,Ord)

instance TensorLike Tuple where toTenExpr (Tuple te) = te; fromTenExpr = Tuple; toPattern = PTenTuple

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
call :: TenFuncName -> [TenExpr] -> TenExpr
call fname args = TenCall fname (map TenArg args)

ecall :: ExprFuncName -> [Expr] -> Expr
ecall nm args = ECall nm args

dimvar :: (Monad m) => StmtT m DimExpr
dimvar = do
  nm <- freshP "var"
  assign_ (PVar nm) (TenDim (DimCtr $ n_get nm))
  return (DimId nm)

shapevar :: (Monad m) => [DimExpr] -> StmtT m ShapeExpr
shapevar de = do
  n <- assignN PShape "shape" (TenShape (foldr1 ShapeSum (map ShapeVector de)))
  return (ShapeTen n)

-- | FIXME: Module returned is only valid in the context of StmtT monad's state.
-- One should encode this fact in types
modul :: (Monad m) => [Function] -> StmtT m Module
modul fns = do
  n <- assignN PFuncTuple "lib" (TenTuple (map unFunction fns))
  return $ Module fns n

-- | FIXME: Convertion from TenExpr to Expr looks weitd. Rethink returning
-- expression from statement monad
axisId :: (Monad m) => Tensor -> Integer -> StmtT m IterVar
axisId (Tensor t) i = IterVar . (\(TenId n) -> EId n) <$> assignN PIterVar "axis" (TenCall TenAxisId [TenArg t, IntArg i])

data IterVar = IterVar Expr
  deriving(Show,Read,Eq,Ord)

-- | FIXME: Convertion from TenExpr to Expr looks weitd. Rethink returning
-- expression from statement monad
reduce_axis :: (Monad m) => (DimExpr,DimExpr) -> StmtT m IterVar
reduce_axis (a,b) = IterVar . (\(TenId n) -> EId n) <$> assignN PIterVar "reduce_axis" (TenCall TenReduceAxis [TenArg $ TenTuple [TenDim a, TenDim b]])

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


class HasDefault a where
  def :: a

{-
 _____ ___  ____ ___   ____  _           _ _
|_   _/ _ \|  _ \_ _| | __ )(_)_ __   __| (_)_ __   __ _ ___
  | || | | | |_) | |  |  _ \| | '_ \ / _` | | '_ \ / _` / __|
  | || |_| |  __/| |  | |_) | | | | | (_| | | | | | (_| \__ \
  |_| \___/|_|  |___| |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
                                                   |___/

FIXME: Bindings are highly C++ - specific, rethink
-}

op1 op (Tensor a) = Tensor $ TenCall (TenOp op) [TenArg a]
op2 op (Tensor a) (Tensor b) = Tensor $ TenCall (TenOp op) [TenArg a, TenArg b]

elemwise1 op (Tensor a) = Tensor $ TenCall (TenElemwise op) [TenArg a]
elemwise2 op (Tensor a) (Tensor b) = Tensor $ TenCall (TenElemwise op) [TenArg a, TenArg b]

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

data Conv2dArgs = Conv2dArgs {
    conv2d_stride :: (Integer,Integer)
  , conv2d_padding ::(Integer,Integer)
  , conv2d_dilation :: (Integer,Integer)
  , conv2d_type :: Type
  , conv2d_name :: Text
  } deriving(Read,Show,Eq,Ord)

instance HasDefault Conv2dArgs where
  def = Conv2dArgs (1,1) (1,1) (1,1) TypeFloat32 "conv2d"

conv2d_nchw :: Tensor -> Tensor -> Conv2dArgs -> Tensor
conv2d_nchw (Tensor x) (Tensor k) Conv2dArgs{..} =
  Tensor $ TenCall TenConv2d_NCHW [
    TenArg x, TenArg k,
    TenArg $ TenDim $ DimConst $ fst conv2d_stride,
    TenArg $ TenDim $ DimConst $ snd conv2d_stride,
    TenArg $ TenDim $ DimConst $ fst conv2d_padding,
    TenArg $ TenDim $ DimConst $ snd conv2d_padding,
    StrArg conv2d_name
    ]

data PadArgs = PadArgs {
    pad_before :: [Expr]
  , pad_after :: [Expr]
  , pad_value :: Expr
  , pad_name :: Text
  } deriving(Read,Show,Eq,Ord)

instance HasDefault PadArgs where
  def = PadArgs [] [] 0 "pad"

pad :: Tensor -> PadArgs -> Tensor
pad (Tensor x) PadArgs{..} =
  Tensor $ TenCall TenPad [
      TenArg x
    , TenArg $ TenExpr $ ETuple pad_before
    , TenArg $ TenExpr $ ETuple pad_after
    , TenArg $ TenExpr $ pad_value
    , StrArg pad_name
    ]

matmul :: Tensor -> Tensor -> Tensor
matmul (Tensor a) (Tensor b) = Tensor $ TenCall TenMatMul [TenArg a, TenArg b]

sigmoid :: Tensor -> Tensor
sigmoid = elemwise1 "sigmoid"

relu :: Tensor -> Tensor
relu = elemwise1 "relu"

split :: Tensor -> [Integer] -> Integer -> Tuple
split (Tensor a) indices axis = Tuple $ TenCall TenSplit [TenArg a, IntsArg indices, IntArg axis]

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

instance TensorLike Schedule where toTenExpr (Schedule s) = s; fromTenExpr = Schedule; toPattern = PSchedule

schedule :: (Monad m) => [Tensor] -> StmtT m Schedule
schedule ts = Schedule <$> assignN PSchedule "sched" (TenCall TenSchedule [TenArg $ TenTuple [t|Tensor t<-ts]])

parallel :: (Monad m) => Schedule -> Tensor -> IterVar -> StmtT m ()
parallel (Schedule s) (Tensor t) (IterVar a) =
  return () <* assignN PStage "stage" (TenCall TenParallel [TenArg s, TenArg t, TenArg $ TenExpr a])

