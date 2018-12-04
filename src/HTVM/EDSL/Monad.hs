{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}
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

assignN :: (Monad m) => (Name -> Pattern) -> Text -> TenExpr -> StmtT m Name
assignN mkpat prefix te1 = do
  n <- freshP prefix
  assign_ (mkpat n) te1
  return n

assign :: (Monad m) => Tensor -> StmtT m Tensor
assign (Tensor te) = Tensor . TenId <$> assignN PTensor "asgn" te

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

function :: (Monad m) => Text -> [Placeholder] -> ([Tensor] -> StmtT m Tensor) -> StmtT m Function
function n plh fbody = do
  Function <$> do
    (\x -> assign_ (PFunc (Name n)) x >> pure (TenId (Name n))) =<< do
      scope $ do
        plhs <- map TenId <$> (forM plh $ assignN PTensor "plh" . TenPlh)
        Tensor bres <- fbody (map Tensor plhs)
        res <- assignN PTenTuple "res" (TenTuple (plhs <> [bres]))
        modify $ \s -> s{sc_expr = \te -> TenDef n ((sc_expr s) te)}
        return (TenId res)

compute' :: ShapeExpr -> Name -> (Expr -> Expr) -> Tensor
compute' se nm body = Tensor $ TenCompute se (PAxis nm) (body (EId nm))

compute :: (Monad m) => ShapeExpr -> (Expr -> Expr) -> StmtT m Tensor
compute se ebody = do
  axis <- freshP "vars"
  assign $ compute' se axis ebody

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
  return (ShapeTen (TenId n))

-- | FIXME: Module returned is only valid in the context of StmtT monad's state.
-- One should encode this fact in types
modul :: (Monad m) => [Function] -> StmtT m Module
modul fns = do
  n <- assignN PFuncTuple "lib" (TenTuple (map unFunction fns))
  return $ Module fns (TenId n)

axisId :: (Monad m) => Tensor -> Integer -> StmtT m IterVar
axisId (Tensor t) i = IterVar . EId <$> assignN PIterVar "axis" (TenCall TenAxisId [TenArg t, TenArgInt i])

data IterVar = IterVar Expr
  deriving(Show,Read,Eq,Ord)

-- | FIXME: Rethink returning expression from statement monad
reduce_axis :: (Monad m) => (DimExpr,DimExpr) -> StmtT m IterVar
reduce_axis (a,b) = IterVar . EId <$> assignN PIterVar "reduce_axis" (TenCall TenReduceAxis [TenArg $ TenTuple [TenDim a, TenDim b]])

infixr 7 !

class Sliceable a b c | a->c, b->c, a->b where
  (!) :: a -> b -> c

instance Sliceable Tensor [Expr] Expr where
  (!) :: Tensor -> [Expr] -> Expr
  (!) (Tensor t) sl = ETenSlice t sl

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
    TenArgStr conv2d_name
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
    , TenArgStr pad_name
    ]

matmul :: Tensor -> Tensor -> Tensor
matmul (Tensor a) (Tensor b) = Tensor $ TenCall TenMatMul [TenArg a, TenArg b]

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
  pi = error "pi is not defined for Tensor"
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

sigmoid :: Tensor -> Tensor
sigmoid (Tensor t) = compute' (ShapeTen t) (Name "s") (\x -> ECall ESigmoid [ ETenSlice t [EId (Name "s")] ])

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

schedule :: (Monad m) => [Tensor] -> StmtT m Schedule
schedule ts = Schedule . TenId <$> assignN PSchedule "sched" (TenCall TenSchedule [TenArg $ TenTuple [t|Tensor t<-ts]])

parallel :: (Monad m) => Schedule -> Tensor -> IterVar -> StmtT m ()
parallel (Schedule s) (Tensor t) (IterVar a) =
  return () <* assignN PStage "stage" (TenCall TenParallel [TenArg s, TenArg t, TenArg $ TenExpr a])

