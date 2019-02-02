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
buildLModule :: CompileConfig -> FilePath -> LModule -> IO (ModuleLib LModule)
buildLModule cc fp m = do
  withTmpf "mgen" $ \fpath -> do
    mgen <- compileModuleGen cc fpath (CPP.printLModuleGen m)
    asm <- runModuleGen mgen
    compileModule fp asm

-- stageBuildModule :: (MonadIO m) => CompileConfig -> FilePath -> StmtT m Module -> m (ModuleLib Module)
-- stageBuildModule cc fp m = stageModuleT m >>= liftIO . buildModule cc fp

-- stageBuildFunction :: (MonadIO m) => CompileConfig -> FilePath -> StmtT m Function -> m (ModuleLib Module)
-- stageBuildFunction cc fp m = stageModuleT (m >>= modul . (\x->[x])) >>= liftIO . buildModule cc fp

-- | Compile and run IR code printer of the current function
-- FIXME: It takes too long to execute this function.
printLFunctionIR :: CompileConfig -> LoweredFunc -> IO Text
printLFunctionIR cc m@(LoweredFunc _ te) = do
  withTmpf "printer" $ \f -> do
    ProgramBin prg <- compileProgram cc f (CPP.printPrinter te)
    let exec_fp = if isAbsolute prg then prg else "./" <> prg
    (ec,out,err) <- readProcessWithExitCode exec_fp [] []
    case ec of
      ExitFailure ec -> do
        error $ "printFunction: compileProgram failed, exit code " <> show ec
      ExitSuccess -> return (tpack out)

-- | Prints the prettified C++ source of the TVM module generator
printLModuleGenCpp :: CompileConfig -> LModule -> IO Text
printLModuleGenCpp _ = prettyCpp . mgen_src . CPP.printLModuleGen

