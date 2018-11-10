module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import Control.Monad(when)
import Data.Functor.Foldable (Fix(..), Recursive(..), Corecursive(..))
import Data.Maybe(fromMaybe)
import Data.Monoid ((<>))

import HTVM



main :: IO ()
main = defaultMain $
    testGroup "All" [
      testCase "FFI" $ do
        return ()
    ]
