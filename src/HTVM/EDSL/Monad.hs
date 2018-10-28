{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module HTVM.EDSL.Monad where

import qualified Data.Text as Text

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
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

name :: (Monad m) => Text -> m Name
name = return . Name

fresh' :: (Monad m) => Text -> Text -> StmtT m Name
fresh' pref suff = StmtT $ state $ \s@StmtCtx{..} -> (Name $ wrap pref <> tshow sc_gen <> wrap suff, s{sc_gen = sc_gen+1})
  where
    wrap x = if x == "" then x else x <> "_"

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

stageStmt :: (Monad m) => StmtT m TenExpr -> m TenExpr
stageStmt s = stage <$> runStmtT initStmtCtx s where
  stage (te,StmtCtx{..}) = sc_expr te

stageLibrary :: (Monad m) => StmtT m Library -> m Library
stageLibrary s = stage <$> runStmtT initStmtCtx s where
  stage (Library te,StmtCtx{..}) = Library $ sc_expr te

assign'' :: (Monad m) => Pattern -> a -> Text -> (a -> TenExpr) -> (Pattern -> a) -> StmtT m a
assign'' p a prefix ctr ctrP = do
  modify $ \s -> s{sc_expr = \te -> (sc_expr s) (TenLet p (ctr a) te)}
  return $ ctrP p

assign' :: (Monad m) => a -> Text -> (a -> TenExpr) -> (Pattern -> a) -> StmtT m a
assign' a prefix ctr ctrP = do
  p <- Pattern <$> freshP prefix
  assign'' p a prefix ctr ctrP

assignN :: (Monad m) => Text -> TenExpr -> StmtT m TenExpr
assignN n te = do
  assign'' (Pattern $ Name n) te n id TenId

assign :: (Monad m) => TenExpr -> StmtT m TenExpr
assign te = assign' te "tensor" id TenId

function :: (Monad m) => Text -> [(Text,Type,ShapeExpr)] -> ([TenExpr] -> StmtT m TenExpr) -> StmtT m Function
function n plh_s fbody = do
  Function <$> do
    assignN n =<< do
      scope $ do
        plhs <- forM plh_s $ (\(n,s,t) -> assign $ TenPlh (Name n,s,t))
        res <- fbody plhs
        modify $ \s -> s{sc_expr = \te -> TenDef (Name n) ((sc_expr s) te)}
        return $ TenTuple (plhs <> [res])

-- | Version of assign where the computation rule is specified for each
-- Tensor's item
compute :: (MonadIO m) => ShapeExpr -> ([DimExpr] -> Expr) -> StmtT m TenExpr
compute shape ebody = do
  p <- Pattern <$> freshP "cpt"
  vars <- map DimId <$> map Pattern <$> mapM (const $ freshP "pat") [1..shapeNDim shape]
  assign $ TenCompute shape p (ebody vars)

-- | Call a function
call :: Text -> Args -> [TenExpr] -> TenExpr
call fname attrs args = TenCall (Name fname) attrs args

(!) :: TenExpr -> [DimExpr] -> Expr
(!) t sl = ESlice t sl

dim :: (Monad m) => StmtT m DimExpr
dim = do
  s <- DimId <$> Pattern <$> freshP "shapevar"
  assign' s "shapevar" TenDim DimId
  return s

shape :: (Monad m) => [DimExpr] -> StmtT m ShapeExpr
shape = return . ShapeConst

library :: (Monad m) => [Function] -> StmtT m Library
library fns = do
  return $ Library $ TenTuple (map unFunction fns)

