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
  let
    go = printShapeExpr
  in
  case se of
    ShapeId _ nm -> printName nm
    ShapeVector de -> "{" <> printDimExpr de <> "}"
    ShapeScalar -> "{}"
    ShapeSum se1 se2 -> "shape_concat("<> go se1 <> "," <> go se2 <> ")"

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
    ETuple es -> "tvm::Array<tvm::Expr>({" <> Text.intercalate "," (map go es) <> "})"

printName :: Name -> Text
printName (Name n) = n -- TODO: escape to make C-compatible

isOpName :: Name -> Bool
isOpName (Name n) = n`Text.isInfixOf`"+-*/"

printPattern :: Pattern -> Text
printPattern p =
  case p of
    PTensor n -> "tvm::Tensor " <> printName n
    PShape n -> "tvm::Array<tvm::Expr> " <> printName n
    PVar n -> "tvm::Var " <> printName n
    PFunc n -> "tvm::LoweredFunc " <> printName n
    PAxis n -> "tvm::Array<tvm::Var> " <> printName n
    PTenTuple n -> "tvm::Array<tvm::Tensor> " <> printName n
    PFuncTuple n -> "tvm::Array<tvm::LoweredFunc> " <> printName n

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
    TenLet pat e1@(TenLet _ _ _) e2 -> printPattern pat <> " = ({" <> go e1 <> "; });\n" <> go e2
    TenLet pat e1 e2 -> printPattern pat <> " = " <> go e1 <> ";\n" <> go e2
    TenAxis ax -> error "printTenExpr: Axis (aka `tvm::IterVar`) is not implemented"
    TenTuple es -> "{" <> Text.intercalate ", " (map go es) <> "}"
    TenDim s -> printDimExpr s
    TenShape s -> printShapeExpr s
    TenCompute sh p e -> "tvm::compute(" <> printShapeExpr sh <> ", tvm::FCompute([=](" <> printPattern p <> ") { return " <> printExpr e <> "; }))"
    TenDef n te ->
      execWriter $ do
        line $ "({"
        line $ "tvm::BuildConfig config = tvm::build_config();"
        line $ "auto args = ({" <> go te <>  "; });"
        line $ "tvm::Schedule s = tvm::create_schedule({args[args.size()-1]->op});"
        line $ "std::unordered_map<tvm::Tensor, tvm::Buffer> binds;"
        line $ "auto f = tvm::Array<tvm::Tensor>(args);"
        line $ "auto lowered = tvm::lower(s, f, \"" <> n <> "\", binds, config);"
        line $ "lowered[0];"
        line $ "})"
    TenCall nm Args{..} es
      | isOpName nm && (length es == 2) -> go (es!!0) <> printName nm <> go (es!!1)
      | isOpName nm && (length es == 1) -> printName nm <> go (es!!0)
      | otherwise -> printName nm <> "(" <> Text.intercalate ", " (map go es) <> ")" -- FIXME: attrs?

line :: (MonadWriter Text m) => Text -> m ()
line x = tell (x <> "\n")


printModule:: Module -> Text
printModule (Module te) =
  execWriter $ do
    line $ "({"
    line $ "tvm::Array<tvm::LoweredFunc> funcs = ({" <> printTenExpr te <> "; });"
    line $ "tvm::BuildConfig config = tvm::build_config();"
    line $ "auto target = tvm::Target::create(\"llvm\"); "
    line $ "auto target_host = tvm::Target::create(\"llvm\");"
    line $ "tvm::runtime::Module mod = tvm::build(funcs, target, target_host, config);"
    line $ "mod;"
    line $ "})"

printModuleGen :: Module -> CppProgram
printModuleGen mod =
  CppProgram $ execWriter $ do
    line $ "#include <iostream>"
    line $ "#include <random>"
    line $ "#include <iomanip>"
    line $ "#include <array>"
    line $ "#include <exception>"

    line $ "#include <tvm/tvm.h>"
    line $ "#include <tvm/operation.h>"
    line $ "#include <tvm/tensor.h>"
    line $ "#include <tvm/build_module.h>"
    line $ "#include <topi/broadcast.h>"
    line $ "#include <topi/nn.h>"
    line $ ""
    line $ "static inline tvm::Array<tvm::Expr> \
        \       shape_concat(const tvm::Array<tvm::Expr> &s1, const tvm::Array<tvm::Expr> &s2) {\
        \   tvm::Array<tvm::Expr> res(s1);\
        \   for(int i=0; i<s2.size(); i++) {\
        \     res.push_back(s2[i]);\
        \   }\
        \   return res;\
        \ }"
    line $ ""
    line $ "int main()"
    line $ "{"
    line $ "auto mod = " <> printModule mod <> ";"
    line $ "std::cout << mod->GetSource(\"asm\") << std::endl;"
    line $ "}"


