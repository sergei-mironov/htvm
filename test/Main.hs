{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (property, conjoin, choose, suchThat, forAll, sublistOf,
                        label, classify, whenFail, counterexample, elements,
                        vectorOf, Gen, Testable, frequency, sized, Property,
                        arbitrary, Arbitrary, listOf)
import Test.QuickCheck.Monadic (forAllM, monadicIO, run, assert)

import Control.Monad (when)
import Data.Functor.Foldable (Fix(..), Recursive(..), Corecursive(..))
import Data.Maybe (fromMaybe)
import Data.Text (isInfixOf)
import Data.Monoid ((<>))
import System.Directory (getTemporaryDirectory)
import System.IO.Temp (withTempFile)

import HTVM.Prelude
import HTVM

genTensorList1 :: (Arbitrary e) => Gen [e]
genTensorList1 = do
    x <- choose (0,10)
    vectorOf x $ arbitrary

genTensorList2 :: (Arbitrary e) => Gen [[e]]
genTensorList2 = do
    x <- choose (0,10)
    y <- choose (0,10)
    vectorOf x $ vectorOf y $ arbitrary

genShape :: Gen [Integer]
genShape = do
  ndim <- choose (0,4)
  vectorOf ndim (choose (0,5))

withTestModule :: Stmt Function -> (ModuleLib -> IO b) -> IO b
withTestModule mf act =
  withTmpf "htvm-test-module" $ \fp -> do
    {- traceM $ "file: " <> fp -}
    act =<< do
      buildModule fp $
        stageModule $ do
          f <- mf
          modul [f]

main :: IO ()
main = defaultMain $
    testGroup "All" [

      testGroup "Uninitialized Tensor FFI should work" $
        let
          go :: forall e . TVMElemType e => [Integer] -> IO ()
          go sh = do
            a <- newEmptyTensor @e sh KDLCPU 0
            assertEqual "poke-peek-2" (tensorNDim a) (ilength sh)
            assertEqual "poke-peek-1" (tensorShape a) sh

          gen :: forall e . TVMElemType e => Property
          gen = forAll genShape $ monadicIO . run . go @e
        in [
          testProperty "Int32"  $ (gen @Int32)
        , testProperty "Word32" $ (gen @Word32)
        , testProperty "Float"  $ (gen @Float)
        , testProperty "Int64"  $ (gen @Int64)
        , testProperty "Word64" $ (gen @Word64)
        , testProperty "Double" $ (gen @Double)
        ]

    , testGroup "Initiallized Tensor FFI should work" $
        let
          go :: forall d i e . (TVMData d i e, Eq e, Eq d, Show d) => d -> IO ()
          go l = do
            a <- newTensor l KDLCPU 0
            assertEqual "poke-peek-1" (tensorNDim a) (tvmDataNDim l)
            assertEqual "poke-peek-2" (tensorShape a) (tvmDataShape l)
            l2 <- peekTensor a
            assertEqual "poke-peek-3" l l2
            return ()

          flatzero :: [[e]] -> [[e]]
          flatzero x | length (concat x) == 0 = []
                     | otherwise = x

          gen1 :: forall e i . (Eq e, Show e, TVMData [e] i e, Arbitrary e) => Property
          gen1 = forAll (genTensorList1 @e) $ monadicIO . run . go

          gen2 :: forall e i . (Eq e, Show e, TVMData [[e]] i e, Arbitrary e) => Property
          gen2 = forAll (genTensorList2 @e) $ monadicIO . run . go . flatzero
        in [
          testProperty "[Int32]"      $ (gen1 @Int32)
        , testProperty "[Word32]"     $ (gen1 @Word32)
        , testProperty "[Float]"      $ (gen1 @Float)
        , testProperty "[Int64]"      $ (gen1 @Int64)
        , testProperty "[Word64]"     $ (gen1 @Word64)
        , testProperty "[Double]"     $ (gen1 @Double)
        , testProperty "[[Int32]]"    $ (gen2 @Int32)
        , testProperty "[[Word32]]"   $ (gen2 @Word32)
        , testProperty "[[Float]]"    $ (gen2 @Float)
        , testProperty "[[Int64]]"    $ (gen2 @Int64)
        , testProperty "[[Word64]]"   $ (gen2 @Word64)
        , testProperty "[[Double]]"   $ (gen2 @Double)
        ]

    , testCase "Compiler (g++ -ltvm) should be available" $ do
        withTmpf "htvm-compiler-test" $ \x -> do
          _ <- compileModuleGen x (ModuleGenSrc undefined "int main() { return 0; }")
          return ()

    , testCase "Pretty-printer (clang-format) should be available" $ do
        _ <- prettyCpp "int main() { return 0; }"
        return ()

    , testCase "Function printer should work" $
        do
        dump <-
          printFunction =<< do
            stageFunction $ do
              s <- shapevar [10]
              function "vecadd" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
                compute s $ \[i] -> a![i] + b![i]
        assertBool "dump should contain 'produce' keyword" $ isInfixOf "produce" dump

    , testCase "Simple model should work" $
        let
          dim0 = 4 :: Integer
          fname = "vecadd"
        in do
        withTestModule (do
          s <- shapevar [fromInteger dim0]
          function fname [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
            compute s $ \[i] -> a![i] + b![i]
          ) $
          \(ModuleLib p _) -> do
            withModule p $ \hmod -> do
            withFunction fname hmod $ \fmod -> do
              a <- newTensor @[Float] [1,2,3,4] KDLCPU 0
              b <- newTensor @[Float] [10,20,30,40] KDLCPU 0
              c <- newEmptyTensor @Float [dim0] KDLCPU 0
              callTensorFunction c fmod [a,b]
              assertEqual "Simple model result" [11,22,33,44::Float] =<< peekTensor c

    ]

