{-# LANGUAGE RankNTypes #-}

module HTVM.TVMRuntime (
    module HTVM.TVMRuntime.FFI
  , module HTVM.TVMRuntime.Types
  , module HTVM.TVMRuntime.TVMData
  , module HTVM.TVMRuntime.Build
  , module HTVM.TVMRuntime.PrinterCPP
  , module HTVM.TVMRuntime
  , module Data.Int
  , module Data.Word
  ) where

import Data.Int (Int8,Int16,Int32,Int64)
import Data.Text(Text)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Map (Map(..))
import Control.Monad.Trans
import Control.Monad (forM)
import System.Process(readCreateProcess,readProcessWithExitCode,shell)
import System.FilePath(isAbsolute)
import System.Exit(ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)

import HTVM.Prelude
import HTVM.EDSL

import HTVM.TVMRuntime.FFI
import HTVM.TVMRuntime.TVMData
import HTVM.TVMRuntime.PrinterCPP
import HTVM.TVMRuntime.Build
import HTVM.TVMRuntime.Types

import qualified HTVM.TVMRuntime.FFI as TVM
import qualified HTVM.TVMRuntime.PrinterCPP as CPP
import qualified Data.Map as Map

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

-- FIXME: what about renaming TVM Module -> TVM Op?
data ModuleHandle = ModuleHandle {
    mh_lib :: ModuleLib LModule -- ^ ModuleLib LModule
  , mh_handle :: TVMModule -- ^ Module handle itself
  , mh_fhandles :: Map Text TVMFunction -- ^ Function handles
  }

loadModule :: ModuleLib LModule -> IO ModuleHandle
loadModule ml@(ModuleLib module_filepath (LModule func_names _)) = do
  mh <- loadTVMModule module_filepath
  fhs <- Map.unions <$> do
    forM func_names $ \fn -> do
      Map.singleton fn <$> loadTVMFunction fn mh
  return $ ModuleHandle ml mh fhs


data Tensor = Tensor {
  tensor_data :: forall a . (Show a, TVMData a) => a
  }

-- | FIXME: Declare the way of handling errors
callModule :: ModuleHandle -> Text -> [Tensor] -> IO ()
callModule mh fname (ret:args) = undefined
  -- tert <- fromTD
  -- callTensorFunction 



-- | Print the IR of the lowered function. In order to do it, compile and run IR
-- printer program
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

