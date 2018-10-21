{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HTVM.EDSL.Printer where

import qualified Data.Text as Text

import Data.Monoid((<>))
import Data.Maybe(fromJust)
import Data.Text(Text)
import HTVM.EDSL.Types

printExpr :: Expr -> Text
printExpr e = undefined

printShape :: Shape -> Text
printShape es = "{" <> Text.intercalate ", " (map printExpr es) <> "}"

printName :: Name -> Text
printName (Name n) = Text.pack n

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
    TenCall nm@Name{..} Args{..} es
      | (Text.pack n_get`Text.isInfixOf`"+-*/") && (length es == 2) -> go (es!!0) <> printName nm <> go (es!!1)
      | (Text.pack n_get`Text.isInfixOf`"+-*/") && (length es == 1) -> printName nm <> go (es!!0)
      | otherwise -> printName nm <> "(" <> Text.intercalate ", " (map go es) <> ")" -- FIXME: attrs?

printFunction :: Function -> Text
printFunction = undefined

printModule :: Module -> Text
printModule = undefined
