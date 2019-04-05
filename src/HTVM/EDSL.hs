{-# LANGUAGE OverloadedStrings #-}
module HTVM.EDSL (
    module HTVM.EDSL.Types
  , module HTVM.EDSL.Monad
  , module HTVM.EDSL.Build
  , module HTVM.EDSL
  ) where

import HTVM.Prelude
import HTVM.EDSL.Types
import HTVM.EDSL.Monad
import HTVM.EDSL.Build

import Control.Monad.Trans
import System.Process(readCreateProcess,readProcessWithExitCode,shell)
import System.FilePath(isAbsolute)
import System.Exit(ExitCode(..))
import Data.Text(Text)
import System.IO.Unsafe (unsafePerformIO)

import qualified HTVM.EDSL.Printer as CPP

-- | Build TVM module @modname@ from EDSL definition.
-- This function executes @g++@ compiler and @clang-format@ pretty-printer. The
-- environment should contain all the settings required for including TVM
-- headers and linking with TVM library.
--
-- `CompileConfig` allows to dump the program source before generation
--
-- In particular, consider reviewing the following variables:
--   - @PATH@ to contain paths to @g++@ and @clang-format@ binaries
--   - @C_INCLUDE_PATH@, @CPLUS_INCLUDE_PATH@ to contain path to folder with
--     TVM headers
--   - @LIBRARY_PATH@, @LD_LIBRARY_PATH@ to contain paths to folder with TVM
--     shared libraries
--
buildLModule :: BackendType -> CompileConfig -> FilePath -> LModule -> IO (ModuleLib LModule)
buildLModule backend_type cc fp m = do
  withTmpf "mgen" $ \fpath -> do
    mgen <- compileModuleGen cc fpath (CPP.printLModuleGen backend_type m)
    asm <- runModuleGen mgen
    compileModule fp asm

-- | Compile and run IR code printer of the current function
-- FIXME: It takes too long to execute this function.
showLFunctionIR :: CompileConfig -> LoweredFunc -> IO Text
showLFunctionIR cc m@(LoweredFunc _ fdef _) = do
  withTmpf "printer" $ \f -> do
    ProgramBin prg <- compileProgram cc f (CPP.printPrinter fdef)
    let exec_fp = if isAbsolute prg then prg else "./" <> prg
    (ec,out,err) <- readProcessWithExitCode exec_fp [] []
    case ec of
      ExitFailure ec -> do
        error $ "printFunction: compileProgram failed, exit code " <> show ec
      ExitSuccess -> return (tpack out)

-- | Prints the function C++ code
showLoweredFuncCpp :: CompileConfig -> LoweredFunc -> IO Text
showLoweredFuncCpp _ = prettyCpp . CPP.printTenExpr . lfuncDefExpr

-- | Prints the prettified C++ source of the TVM module generator
showLModuleGenCpp :: BackendType -> CompileConfig -> LModule -> IO Text
showLModuleGenCpp backend_type _ = prettyCpp . mgs_src . CPP.printLModuleGen backend_type
