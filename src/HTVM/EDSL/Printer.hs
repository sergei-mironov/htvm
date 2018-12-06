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
import HTVM.EDSL.Monad

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
    ShapeTen te -> "htvm_shape("<> printTenExpr te <> ")"
    ShapeVector de -> "{" <> printDimExpr de <> "}"
    ShapeScalar -> "{}"
    ShapeSum se1 se2 -> "htvm_shape_concat("<> go se1 <> "," <> go se2 <> ")"

printExprFuncName :: ExprFuncName -> Text
printExprFuncName fn =
  case fn of
    ExprSum -> "tvm::sum"
    ExprOp op -> op
    ESigmoid -> "tvm::sigmoid"

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
    ECall nm es ->
      case nm of
        ExprOp _
          | length es == 2 -> go (es!!0) <> printExprFuncName nm <> go (es!!1)
          | length es == 1 -> printExprFuncName nm <> go (es!!0)
        _ -> printExprFuncName nm <> "(" <> Text.intercalate "," (map go es) <> ")"
    ETenSlice te es -> printTenExpr te <> "(" <> Text.intercalate "," (map go es) <> ")"
    EShapeSlice se sl -> printShapeExpr se <> "[" <> tshow sl <> "]"
    ESlice e i -> go e <> "[" <> tshow i <> "]"
    ETuple es -> "{" <> Text.intercalate "," (map go es) <> "}"

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
    PIterVar n -> "tvm::IterVar " <> printName n
    PFunc n -> "tvm::LoweredFunc " <> printName n
    PAxis n -> "tvm::Array<tvm::Var> " <> printName n
    PTenTuple n -> "tvm::Array<tvm::Tensor> " <> printName n
    PFuncTuple n -> "tvm::Array<tvm::LoweredFunc> " <> printName n
    PSchedule n -> "tvm::Schedule " <> printName n
    PStage n -> "tvm::Stage " <> printName n

printType :: Type -> Text
printType t =
  case t of
    TypeFloat32 -> "tvm::Float(32)"
    TypeInt32 ->  "tvm::Int(32)"
    TypeTensor _ _ -> "tvm::Tensor()"

printTenFuncName :: TenFuncName -> Text
printTenFuncName fn =
  case fn of
    TenOp op -> op
    TenReduceAxis -> "tvm::reduce_axis"
    TenConv2d_NCHW -> "topi::conv2d_nchw"
    TenPad -> "topi::pad"
    TenSchedule -> "htvm_create_schedule"
    TenParallel -> "htvm_parallel"
    TenAxisId -> "htvm_axis_id"
    TenMatMul -> "topi::matmul"
    TenElemwise x -> "topi::"<>x
    TenSplit -> "topi::split"

printLayout :: Layout -> Text
printLayout l =
  case l of
    NCHW -> "NCHW"
    NWCN -> "NWCN"
    NHWC -> "NHWC"

printTenExpr :: TenExpr -> Text
printTenExpr te =
  let
    go = printTenExpr

    parg arg =
      case arg of
        TenArg e -> go e
        StrArg str -> "\"" <> str <> "\""
        TypeArg t -> printType t
        IntArg i -> tshow i
        IntsArg is -> "{" <> (Text.intercalate "," (map tshow is)) <> "}"
        LayoutArg l -> printLayout l
  in
  case te of
    TenPlh (n,ty,s) -> "tvm::placeholder(" <> printShapeExpr s <> "," <> printType ty <> ",\""<>n<>"\")"
    TenId n -> printName n
    TenLet pat e1@(TenLet _ _ _) e2 -> printPattern pat <> " = ({" <> go e1 <> "; });\n" <> go e2
    TenLet pat e1 e2 -> printPattern pat <> " = " <> go e1 <> ";\n" <> go e2
    -- TenAxis (a,b) -> --error "printTenExpr: Axis (aka `tvm::IterVar`) is not implemented"
    --               "tvm::reduce_axis({ " <> printDimExpr a <> "," <> printDimExpr b <> "})"
    TenTuple es -> "{" <> Text.intercalate ", " (map go es) <> "}"
    TenSlice te i -> go te <> "[" <> tshow i <> "]"
    TenDim s -> printDimExpr s
    TenShape s -> printShapeExpr s
    TenExpr e -> printExpr e
    TenCompute sh p e -> "tvm::compute(" <> printShapeExpr sh <> ", tvm::FBatchCompute([=](" <> printPattern p <> ") { return tvm::Array<tvm::Expr>(" <> printExpr e <> "); }))"
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
    TenCall nm es ->
      case nm of
        TenOp _
          | (length es == 2) -> parg (es!!0) <> printTenFuncName nm <> parg (es!!1)
          | (length es == 1) -> printTenFuncName nm <> parg (es!!0)
        _ -> printTenFuncName nm <> "(" <> Text.intercalate ", " (map parg es) <> ")" -- FIXME: attrs?

line :: (MonadWriter Text m) => Text -> m ()
line x = tell (x <> "\n")


printModule:: Module -> Text
printModule (Module _ te) =
  execWriter $ do
    line $ "({"
    line $ "tvm::Array<tvm::LoweredFunc> funcs = ({" <> printTenExpr te <> "; });"
    line $ "tvm::BuildConfig config = tvm::build_config();"
    line $ "auto target = tvm::Target::create(\"llvm\"); "
    line $ "auto target_host = tvm::Target::create(\"llvm\");"
    line $ "tvm::runtime::Module mod = tvm::build(funcs, target, target_host, config);"
    line $ "mod;"
    line $ "})"

printIncludes :: Writer Text ()
printIncludes = do
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
    line $ "#include <topi/elemwise.h>"
    line $ "#include <topi/transform.h>"
    line $ ""
    line $
        "static inline tvm::Array<tvm::Expr> \
        \htvm_shape_concat(const tvm::Array<tvm::Expr> &s1, const tvm::Array<tvm::Expr> &s2) {\
        \\
        \  tvm::Array<tvm::Expr> res(s1);\
        \  for(int i=0; i<s2.size(); i++) {\
        \    res.push_back(s2[i]);\
        \  }\
        \\
        \  return res;\
        \}"
    line $
        "tvm::Schedule \
        \htvm_create_schedule(const tvm::Array<tvm::Tensor> &arr) {\
        \  tvm::Array<tvm::Operation> ops;\
        \  for(auto a : arr) { ops.push_back(a->op); }\
        \  return tvm::create_schedule(ops);\
        \}"
    line $ "tvm::Stage htvm_parallel(tvm::Schedule s, tvm::Tensor t, tvm::IterVar i) { return s[t->op].parallel(i); }"
    line $ "tvm::IterVar htvm_axis_id(tvm::Tensor t, int i) { return t->op->root_iter_vars()[i]; }"
    line $ "tvm::Array<tvm::Expr> htvm_shape(tvm::Tensor t) { return t->shape; }"
    line $ "tvm::Array<tvm::Expr> htvm_shape(tvm::Array<tvm::Expr> t) { return t; }"
    line $ ""


printModuleGen :: Module -> ModuleGenSrc
printModuleGen mod =
  ModuleGenSrc mod $ execWriter $ do
    printIncludes
    line $ "int main()"
    line $ "{"
    line $ "auto mod = " <> printModule mod <> ";"
    line $ "std::cout << mod->GetSource(\"asm\") << std::endl;"
    line $ "}"


printPrinter :: TenExpr -> ProgramSrc
printPrinter te =
  ProgramSrc $ execWriter $ do
    printIncludes
    line $ "int main()"
    line $ "{"
    line $ "auto te = ({" <> printTenExpr te <> "; });"
    line $ "std::cout << te->body << std::endl;"
    line $ "}"


