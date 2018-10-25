{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module HTVM.EDSL.Printer where

import qualified Data.Text as Text

import Control.Monad.Writer(MonadWriter,Writer,tell,execWriter)
import Data.Monoid((<>))
import Data.Maybe(fromJust)
import Data.Text(Text)
import HTVM.EDSL.Types


tshow :: (Show a) => a -> Text
tshow = Text.pack . show

tpack :: String -> Text
tpack = Text.pack



printExpr :: Expr -> Text
printExpr e =
  let
    go = printExpr
  in
  case e of
    EConst c ->
      case c of
        CInt i -> tshow i
        CFloat32 f -> tshow f
    EShapeVar sh -> printShapeVar sh
    EAxis a -> printAxis a
    ECall nm es
      | isOpName nm && length es == 2 -> go (es!!0) <> printName nm <> go (es!!1)
      | isOpName nm && length es == 1 -> printName nm <> printExpr (es!!0)
      | otherwise -> printName nm <> "(" <> Text.intercalate "," (map go es) <> ")"
    ESlice te es -> printTenExpr te <> "[" <> Text.intercalate "," (map go es) <> "]"

printAxis :: Axis -> Text
printAxis (GlobalAxis nm) = printName nm
printAxis (LocalAxis i) = tshow i

printShapeVar :: ShapeVar -> Text
printShapeVar (ShapeVar nm) = printName nm

printShape :: Shape -> Text
printShape es = "{" <> Text.intercalate ", " (map printExpr es) <> "}"

printName :: Name -> Text
printName (Name n) = Text.pack n

isOpName :: Name -> Bool
isOpName (Name n) = (tpack n)`Text.isInfixOf`"+-*/"

printTenExpr :: TenExpr -> Text
printTenExpr te =
  let
    go = printTenExpr
  in
  case te of
    TenPlh (n,_,_) -> printName n
    TenId pat -> printName (p_name pat)
    TenLet pat e1 e2 -> printName (p_name pat) <> " = " <> go e1 <> ";" <> go e2
    TenCompute Args{..} e -> "compute(" <> printShape (fromJust a_shape) <> ", [=](i) {" <> printExpr e <> "})"
    TenCall nm Args{..} es
      | isOpName nm && (length es == 2) -> go (es!!0) <> printName nm <> go (es!!1)
      | isOpName nm && (length es == 1) -> printName nm <> go (es!!0)
      | otherwise -> printName nm <> "(" <> Text.intercalate ", " (map go es) <> ")" -- FIXME: attrs?

line :: (MonadWriter Text m) => Text -> m ()
line x = tell (x <> "\n")

printFunction :: Function -> Text
printFunction Function{..} =
  execWriter $ do
    line $ "({"
    line $ "tvm::Tensor C = ({"<> printTenExpr fun_body <>"});"
    line $ "tvm::Schedule s = tvm::create_schedule({C->op});"
    line $ "auto args = tvm::Array<tvm::Tensor>({" <> Text.intercalate "," (map (printName . pls_name) fun_pls) <> "});"
    line $ "auto lowered = tvm::lower(s, args, " <> printName fun_name <> ", binds, config);"
    line $ "lowered;"
    line $ ")}"

printModule :: Module -> Text
printModule (Module nm [f]) = -- FIXME: support more than 1 func
  execWriter $ do
    line $ "({"
    line $ "auto lowered = "<> printFunction f <> ";"
    line $ "tvm::BuildConfig config = tvm::build_config();"
    line $ "auto target = tvm::Target::create(\"llvm\"); "
    line $ "auto target_host = tvm::Target::create(\"llvm\");"
    line $ "tvm::runtime::Module mod = tvm::build(lowered, target, target_host, config);"
    line $ "mod;"
    line $ "})"




