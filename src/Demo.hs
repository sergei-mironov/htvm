{-# LANGUAGE OverloadedStrings #-}

module Demo where

import qualified Data.Text.IO as Text

import Control.Monad.Trans

import HTVM.Prelude
import HTVM

tputStrLn = Text.putStrLn

main :: IO ()
main = do
  return ()

demo1 :: IO Library
demo1 =
  stageLibrary $ do
    n <- dimvar
    s <- shapevar [n]
    library =<< do
      sequence [
          function "vecadd" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
            s2 <- dimvar
            liftIO $ putStrLn "Yarr!"
            c <- compute s $ \[i] -> a![i] + b![i]
            c <- compute s $ \[i] -> a![i] + b![i]
            d <- compute (s<>s) $ \[i,j] -> c![i,s!(0::Integer)] * c![i+i,2*i]
            -- ax1 <- reduce
            -- e <- compute s $ \[i] -> d![ax1,i] * c![i]
            f <- assign $ call "topi.relu" nullArgs [d]
            return f
        ]

demo3 :: IO TenExpr
demo3 = do
  stageStmt $ do
    d <- dimvar
    s <- shapevar [d]
    f <- function "vecadd" [("A",float32,s)] $ \[a] -> do
      compute s $ \[i] -> a![i]
    return $ unFunction f


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

putLib m = tputStrLn =<< printLibrary <$> m

putTenExpr te = tputStrLn =<< printTenExpr <$> te
