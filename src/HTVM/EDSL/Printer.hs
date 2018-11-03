{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module HTVM.EDSL.Printer where

import qualified Data.Text as Text

import Control.Monad.Writer(MonadWriter,Writer,tell,execWriter)
import Data.Monoid((<>))
import Data.Maybe(fromJust)
import Data.Text(Text)

import HTVM.Prelude
import HTVM.EDSL.Types

printDimExpr :: DimExpr -> Text
printDimExpr se =
  let
    go = printDimExpr
  in
  case se of
    DimConst i -> tshow i
    DimId n -> printName n
    DimCall nm es
      | isOpName nm && length es == 2 -> go (es!!0) <> printName nm <> go (es!!1)
      | isOpName nm && length es == 1 -> printName nm <> go (es!!0)
      | otherwise -> printName nm <> "(" <> Text.intercalate "," (map go es) <> ")"
    DimCtr n -> "tvm::var(\"" <> n <> "\")"

printShapeExpr :: ShapeExpr -> Text
printShapeExpr se =
  case se of
    ShapeId _ nm -> printName nm
    ShapeVector de -> "{" <> printDimExpr de <> "}"
    ShapeScalar -> "{}"
    ShapeSum se1 se2 -> printShapeExpr se1 <> "+" <> printShapeExpr se2

printExpr :: Expr -> Text
printExpr e =
  let
    go = printExpr
  in
  case e of
    EId n -> printName n
    EConst c ->
      case c of
        CInt i -> tshow i
        CFloat32 f -> tshow f
    ECall nm es
      | isOpName nm && length es == 2 -> go (es!!0) <> printName nm <> go (es!!1)
      | isOpName nm && length es == 1 -> printName nm <> go (es!!0)
      | otherwise -> printName nm <> "(" <> Text.intercalate "," (map go es) <> ")"
    ETenSlice te es -> printTenExpr te <> "(" <> Text.intercalate "," (map go es) <> ")"
    EShapeSlice se sl -> printShapeExpr se <> "[" <> tshow sl <> "]"
    ETuple es -> "{" <> Text.intercalate "," (map go es) <> "}"

printName :: Name -> Text
printName (Name n) = n -- TODO: escape to make C-compatible

isOpName :: Name -> Bool
isOpName (Name n) = n`Text.isInfixOf`"+-*/"

printPattern :: Pattern -> Text
printPattern (Pattern n) = printName n

printType :: Type -> Text
printType t =
  case t of
    TypeFloat32 -> "tvm::Float(32)"
    TypeInt32 ->  "tvm::Int(32)"
    Tensor _ _ -> "tvm::Tensor()"

printTenExpr :: TenExpr -> Text
printTenExpr te =
  let
    go = printTenExpr
  in
  case te of
    TenPlh (n,ty,s) -> "tvm::placeholder(" <> printShapeExpr s <> "," <> printType ty <> ",\""<>n<>"\")"
    TenId n -> printName n
    TenLet pat e1@(TenLet _ _ _) e2 -> "auto " <> printName (p_name pat) <> " = ({" <> go e1 <> "; });\n" <> go e2
    TenLet pat e1 e2 -> "auto " <> printName (p_name pat) <> " = " <> go e1 <> ";\n" <> go e2
    TenTuple es -> "{" <> Text.intercalate ", " (map go es) <> "}"
    TenDim s -> printDimExpr s
    TenShape s -> printShapeExpr s
    TenCompute sh p e -> "tvm::compute(" <> printShapeExpr sh <> ", [=](" <> printPattern p <> ") { return " <> printExpr e <> "; })"
    TenDef n te ->
      execWriter $ do
        line $ "({"
        line $ "tvm::BuildConfig config = tvm::build_config();"
        line $ "auto args = ({" <> go te <>  "; });"
        line $ "tvm::Schedule s = tvm::create_schedule({args[args.size()-1]->op});"
        line $ "std::unordered_map<tvm::Tensor, tvm::Buffer> binds;"
        line $ "auto f = tvm::Array<tvm::Tensor>(args);"
        line $ "auto lowered = tvm::lower(s, f, " <> printName n <> ", binds, config);"
        line $ "lowered[0];"
        line $ "})"

    TenCall nm Args{..} es
      | isOpName nm && (length es == 2) -> go (es!!0) <> printName nm <> go (es!!1)
      | isOpName nm && (length es == 1) -> printName nm <> go (es!!0)
      | otherwise -> printName nm <> "(" <> Text.intercalate ", " (map go es) <> ")" -- FIXME: attrs?

line :: (MonadWriter Text m) => Text -> m ()
line x = tell (x <> "\n")

printMain :: Text -> Text
printMain mod =
  execWriter $ do
    line $ "#include <random>"
    line $ "#include <iomanip>"
    line $ "#include <array>"
    line $ "#include <exception>"

    line $ "#include <tvm/tvm.h>"
    line $ "#include <tvm/operation.h>"
    line $ "#include <tvm/tensor.h>"
    line $ "#include <tvm/build_module.h>"
    line $ "#include <topi/broadcast.h>"

    line $ "int main()"
    line $ "{"
    line $ "auto mod = " <> mod <> ";"
    line $ "std::cout << mod->GetSource(\"asm\") << std::endl;"
    line $ "}"

printLibrary :: Library -> Text
printLibrary (Library te) =
  execWriter $ do
    line $ "({"
    line $ "auto funcs = ({" <> printTenExpr te <> "; });"
    line $ "tvm::BuildConfig config = tvm::build_config();"
    line $ "auto target = tvm::Target::create(\"llvm\"); "
    line $ "auto target_host = tvm::Target::create(\"llvm\");"
    line $ "tvm::runtime::Module mod = tvm::build(funcs, target, target_host, config);"
    line $ "mod;"
    line $ "})"


printProgram :: Library -> Text
printProgram l = printMain (printLibrary l)


