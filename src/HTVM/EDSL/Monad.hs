{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module HTVM.EDSL.Monad where


-- TODO: Reweite in Applicative, learn ApplicativeLift approach

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromMaybe,fromJust)

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
    sc_expr :: Maybe TenExpr
  } deriving(Show)

initStmtCtx = StmtCtx Nothing

newtype StmtT m a = StmtT { unStmtT :: StateT StmtCtx m a }
  deriving(Functor,Applicative,Monad,MonadTrans)

runStmtT :: (Monad m) => StmtT m TenExpr -> m (TenExpr,StmtCtx)
runStmtT s = flip runStateT initStmtCtx $ unStmtT s

stageStmt :: (Monad m) => StmtT m TenExpr -> m TenExpr
stageStmt s = fst <$> runStmtT s

function :: (Monad m) => Name -> [Placeholder] -> ([TenExpr] -> StmtT m TenExpr) -> m Function
function name pls fbody =
  Function <$> pure name <*> pure pls <*> stageStmt (fbody (map TenPlh pls))

-- | Compute a tensor by specifying explicit expression
compute :: (Monad m) => Args -> ([Axis] -> ExprT m Expr) -> StmtT m TenExpr
compute a@Args{..} ebody =
  TenCompute <$> pure a <*> (lift (stageExpr (ebody localAxis)))
  where
    localAxis :: [Axis]
    localAxis = [LocalAxis (toInteger i) | i <- [0..length shape]]

    shape :: Shape
    shape = fromMaybe (error "compute: shape is required") a_shape

-- | Call a TOPI function
topi :: (Monad m) => Name -> Args -> [TenExpr] -> StmtT m TenExpr
topi nm attrs args = return $ TenCall nm attrs args





