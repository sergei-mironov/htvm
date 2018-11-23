{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
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

assign_ :: (Monad m) => Pattern -> TenExpr -> StmtT m ()
assign_ p te1 = do
  modify $ \s -> s{sc_expr = \te -> (sc_expr s) (TenLet p te1 te)}

assignN :: (Monad m) => (Name -> Pattern) -> Text -> TenExpr -> StmtT m Name
assignN mkpat prefix te1 = do
  n <- freshP prefix
  assign_ (mkpat n) te1
  return n

assign :: (Monad m) => TenExpr -> StmtT m TenExpr
assign te = TenId <$> assignN PTensor "asgn" te

function :: (Monad m) => Text -> [Placeholder] -> ([TenExpr] -> StmtT m TenExpr) -> StmtT m Function
function n plh fbody = do
  Function <$> do
    (\x -> assign_ (PFunc (Name n)) x >> pure (TenId (Name n))) =<< do
      scope $ do
        plhs <- map TenId <$> (forM plh $ assignN PTensor "plh" . TenPlh)
        bres <- fbody plhs
        res <- assignN PTenTuple "res" (TenTuple (plhs <> [bres]))
        modify $ \s -> s{sc_expr = \te -> TenDef n ((sc_expr s) te)}
        return (TenId res)

-- | Version of assign where the computation rule is specified for each
-- Tensor's item
compute :: (Monad m) => ShapeExpr -> ([Expr] -> Expr) -> StmtT m TenExpr
compute se ebody = do
  res <- freshP "computed"
  axis <- freshP "vars"
  axis_dims <- pure $ map (EShapeSlice (ShapeId (shapeDim se) axis)) [0..(shapeDim se)-1]
  assign_ (PTensor res) (TenCompute se (PAxis axis) (ebody axis_dims))
  return (TenId res)

-- | Call a function
call :: Text -> Args -> [TenExpr] -> TenExpr
call fname attrs args = TenCall (Name fname) attrs args

ecall :: Text -> [Expr] -> Expr
ecall nm args = ECall (Name nm) args

dimvar :: (Monad m) => StmtT m DimExpr
dimvar = do
  nm <- freshP "var"
  assign_ (PVar nm) (TenDim (DimCtr $ n_get nm))
  return (DimId nm)

shapevar :: (Monad m) => [DimExpr] -> StmtT m ShapeExpr
shapevar de = do
  n <- assignN PShape "shape" (TenShape (foldr1 ShapeSum (map ShapeVector de)))
  return (ShapeId (toInteger $ length de) n)

-- | FIXME: Module returned is only valid in the context of StmtT monad's state.
-- One should encode this fact in types
modul :: (Monad m) => [Function] -> StmtT m Module
modul fns = do
  n <- assignN PFuncTuple "lib" (TenTuple (map unFunction fns))
  return $ Module fns (TenId n)

-- | FIXME: Rethink returning expression from statement monad
axis :: (Monad m) => (DimExpr,DimExpr) -> StmtT m Expr
axis (a,b) = do
  n <- assignN PIterVar "axis" (TenAxis (a,b))
  return (EId n)


class Sliceable a b c | a->c, b->c, a->b where
  (!) :: a -> b -> c

instance Sliceable TenExpr [Expr] Expr where
  (!) :: TenExpr -> [Expr] -> Expr
  (!) t sl = ETenSlice t sl

instance Sliceable ShapeExpr Integer Expr where
  (!) :: ShapeExpr -> Integer -> Expr
  (!) t sl = EShapeSlice t sl


