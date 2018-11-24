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
      buildModule defaultConfig fp $
        stageModule $ do
          f <- mf
          modul [f]

main :: IO ()
main = defaultMain $
    testGroup "All" $ reverse [

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
          _ <- compileModuleGen defaultConfig x (ModuleGenSrc undefined "int main() { return 0; }")
          return ()

    , testCase "Pretty-printer (clang-format) should be available" $ do
        _ <- prettyCpp "int main() { return 0; }"
        return ()

    , testCase "Function printer should work" $
        do
        dump <-
          printFunction defaultConfig =<< do
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

    , testCase "Reduce axis operation should compile" $

        withTestModule (do
          s <- shapevar [4]
          function "reduce" [("A",float32,s)] $ \[a] -> do
            r <- axis (0,3)
            compute ShapeScalar $ \[] -> esum (a![r], [r])
          ) $ \_ -> return ()

    , testCase "Conv2d operation should compile" $

        withTestModule (do
          sa <- shapevar [1,1,10,10]
          sk <- shapevar [1,1,3,3]
          function "reduce" [("A",float32,sa), ("k",float32,sk)] $ \[a,k] -> do
            conv2d_nchw a k def
          ) $ \_ -> return ()

    , testCase "Pad operation should compile" $

        withTestModule (do
          sa <- shapevar [1,1,10,10]
          function "reduce" [("A",float32,sa) ] $ \[a] -> do
            pad a def{pad_value=33, pad_before=[2,2,2,2]}
          ) $ \_ -> return ()
    ]

