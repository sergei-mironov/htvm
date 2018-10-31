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

stageStmt :: (Monad m) => StmtT m TenExpr -> m TenExpr
stageStmt s = stage <$> runStmtT initStmtCtx s where
  stage (te,StmtCtx{..}) = sc_expr te

stageLibrary :: (Monad m) => StmtT m Library -> m Library
stageLibrary s = stage <$> runStmtT initStmtCtx s where
  stage (Library te,StmtCtx{..}) = Library $ sc_expr te

-- assign'' :: (Monad m) => Pattern -> a -> Text -> (a -> TenExpr) -> (Pattern -> a) -> StmtT m a
-- assign'' p a prefix ctr ctrP = do
--   modify $ \s -> s{sc_expr = \te -> (sc_expr s) (TenLet p (ctr a) te)}
--   return $ ctrP p

-- assign' :: (Monad m) => a -> Text -> (a -> TenExpr) -> (Pattern -> a) -> StmtT m Name
-- assign' a prefix ctr ctrP = do
--   p <- Pattern <$> freshP prefix
--   assign'' p a prefix ctr ctrP

-- assignN :: (Monad m) => Name -> TenExpr -> StmtT m ()
-- assignN n te1 = do

assign_ :: (Monad m) => Name -> TenExpr -> StmtT m ()
assign_ n te1 = do
  modify $ \s -> s{sc_expr = \te -> (sc_expr s) (TenLet (Pattern n) te1 te)}

assignN :: (Monad m) => Text -> TenExpr -> StmtT m Name
assignN prefix te1 = do
  n <- freshP prefix
  assign_ n te1
  return n

assign :: (Monad m) => TenExpr -> StmtT m TenExpr
assign te = TenId <$> assignN "asgn" te

function :: (Monad m) => Text -> [Placeholder] -> ([TenExpr] -> StmtT m TenExpr) -> StmtT m Function
function n plh fbody = do
  Function <$> do
    (\x -> assign_ (Name n) x >> pure (TenId (Name n))) =<< do
      scope $ do
        plhs <- map TenId <$> (forM plh $ assignN "plh" . TenPlh)
        res <- fbody plhs
        modify $ \s -> s{sc_expr = \te -> TenDef (Name n) ((sc_expr s) te)}
        return $ TenTuple (plhs <> [res])

-- | Version of assign where the computation rule is specified for each
-- Tensor's item
compute :: (MonadIO m) => ShapeExpr -> ([Expr] -> Expr) -> StmtT m TenExpr
compute se ebody = do
  nm <- freshP "cpt"
  dims <- pure $ map (EShapeSlice (ShapeId (shapeDim se) nm)) [1..shapeDim se]
  assign_ nm (TenCompute se (Pattern nm) (ebody dims))
  return (TenId nm)

-- | Call a function
call :: Text -> Args -> [TenExpr] -> TenExpr
call fname attrs args = TenCall (Name fname) attrs args

dimvar :: (Monad m) => StmtT m DimExpr
dimvar = do
  n <- assignN "var" (TenDim (DimCall (Name "tvm::var") []))
  return (DimId n)

shapevar :: (Monad m) => [DimExpr] -> StmtT m ShapeExpr
shapevar de = do
  n <- assignN "shape" (TenShape (foldr1 ShapeSum (map ShapeVector de)))
  return (ShapeId (toInteger $ length de) n)

library :: (Monad m) => [Function] -> StmtT m Library
library fns = do
  return $ Library $ TenTuple (map unFunction fns)


class Sliceable a b c | a->c, b->c where
  (!) :: a -> b -> c

instance Sliceable TenExpr [Expr] Expr where
  (!) :: TenExpr -> [Expr] -> Expr
  (!) t sl = ETenSlice t sl

instance Sliceable ShapeExpr Integer Expr where
  (!) :: ShapeExpr -> Integer -> Expr
  (!) t sl = EShapeSlice t sl


