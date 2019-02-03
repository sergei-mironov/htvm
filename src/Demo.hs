{-# LANGUAGE OverloadedStrings #-}

module Demo where

import Control.Monad.Trans
import HTVM.Prelude
import HTVM

main :: IO ()
main = do
  return ()

demo1 :: IO LModule
demo1 =
  stageStmtT $ do
    n <- dimvar
    s <- shapevar [10]
    x <- dimvar
    modul =<< do
      sequence [
          function "foo" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
            s2 <- dimvar
            c <- compute s $ \i -> a![i] + b![i]
            c <- compute s $ \i -> a![i] + b![i]
            d <- compute (s<>s) $ \(i,j) -> c![s!0] * c![i+j]
            f <- assign $ relu d
            return f

        , function "bar" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
            c <- compute s $ \i -> a![i] * b![i]
            return c
        ]

{-
demo3 :: IO TenExpr
demo3 = do
  stageTenExpr $ do
    d <- dimvar
    s <- shapevar [d,10]
    f <- function "vecadd" [("A",float32,s)] $ \[a] -> do
      compute s $ \[i,j] -> a![i,10]
    return $ unFunction f

-}

-- demo4 :: IO TenExpr
-- demo4 = do
--   stageStmt $ do
--     x <- somevar "x"
--     s <- shape [10,x]
--     f <- function "vecadd" [("A",float32,s)] $ \[a] -> do
--       k <- reducevar (0,x)
--       compute s (\[i] -> sum (\[k] -> a![i,k]) [k,2])
--     return $ unFunction f


-- demo2 :: IO Module
-- demo2 =
--   let
--     s = shape [20]
--   in do
--   Module <$> name "vecadd" <*> sequence [
--       function "vecadd" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
--         liftIO $ putStrLn "Yarr!"
--         c <- compute s $ \[i] -> a![i] + b![i]
--         d <- compute [s!!0,s!!0] $ \[i,j] -> c![i,i] * c![i,i]
--         e <- assign $ call "topi.relu" nullArgs [d]
--         return c

--     , function "vecfoo" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
--         c <- compute s $ \[i] -> (call "topi.relu" nullArgs [a])![i] + b![i]
--         return c
--     ]

printLib m = tputStrLn =<< (prettyCpp . mgen_src . printModuleGen) =<< m

-- writeLib m = twriteFile "htvm.cpp" =<< prettyCpp =<< printPrinter <$> m

putTenExpr te = tputStrLn =<< (prettyCpp $ printTenExpr te)
