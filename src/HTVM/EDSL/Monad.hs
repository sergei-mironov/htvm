{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module HTVM.EDSL.Monad where


-- TODO: Reweite in Applicative, learn ApplicativeLift approach

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
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
    sc_namegen :: Integer
  , sc_expr :: TenExpr -> TenExpr
  }

initStmtCtx = StmtCtx 0 id

newtype StmtT m a = StmtT { unStmtT :: StateT StmtCtx m a }
  deriving(Functor,Applicative,Monad,MonadTrans,MonadState StmtCtx,MonadIO)

name :: (Monad m) => String -> m Name
name = return . Name

fresh :: (Monad m) => StmtT m Name
fresh = StmtT $ state $ \s@StmtCtx{..} -> (Name $ "x" <> show sc_namegen, s{sc_namegen = sc_namegen+1})


runStmtT :: (Monad m) => StmtT m TenExpr -> m (TenExpr,StmtCtx)
runStmtT s = flip runStateT initStmtCtx $ unStmtT s

stageStmt :: (Monad m) => StmtT m TenExpr -> m TenExpr
stageStmt s = stage <$> runStmtT s where
  stage (te,StmtCtx{..}) = sc_expr te

function :: (Monad m) => String -> [(String,Type,Shape)] -> ([TenExpr] -> StmtT m TenExpr) -> m Function
function n plh_s fbody =
  Function <$> name n <*> pure plh <*> stageStmt (fbody (map TenPlh plh)) where
    plh = map (\(n,s,t) -> (Name n,s,t)) plh_s

-- | Assigns expression a name
assign :: (Monad m) => TenExpr -> StmtT m TenExpr
assign te0 = do
  p <- Pattern <$> fresh
  modify $ \s -> s{sc_expr = \te -> (sc_expr s) (TenLet p te0 te)}
  return $ TenId p

-- | Version of assign where the computation rule is specified for each
-- Tensor's item
compute :: (MonadIO m) => Shape -> ([Expr] -> Expr) -> StmtT m TenExpr
compute shape ebody =
  let
    localAxis :: [Expr]
    localAxis = [EAxis $ LocalAxis (toInteger i) | i <- [1..length shape]]
  in do
  assign $ TenCompute nullArgs{a_shape=Just shape} (ebody localAxis)

-- | Call a function
call :: String -> Args -> [TenExpr] -> TenExpr
call fname attrs args = TenCall (Name fname) attrs args

(!) :: TenExpr -> [Expr] -> Expr
(!) t sl = ESlice t sl

