{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import Control.Monad(when)
import Data.Functor.Foldable (Fix(..), Recursive(..), Corecursive(..))
import Data.Maybe(fromMaybe)
import Data.Monoid ((<>))

import HTVM.Prelude
import HTVM



main :: IO ()
main = defaultMain $
    testGroup "All" [
      testCase "FFI" $ do
        withModule "model.so" $ \pmod -> do
        withFunction "vecadd" pmod $ \_ -> do
        with_tvmTensor ([1.0, 2.0, 3.0, 4.0] :: [Float]) KDLCPU 0 $ \_ -> do
          tputStrLn "Inside!"
        return ()
    ]

