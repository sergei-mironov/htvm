module HTVM.EDSL.Build where

import Data.Text(Text)
import System.FilePath(isAbsolute)
import System.Exit(ExitCode(..))
import System.Process(readCreateProcess,readProcessWithExitCode,shell)
import System.IO (hPutStr,stdout,stderr)

import HTVM.Prelude
import HTVM.EDSL.Types
import HTVM.EDSL.Monad
import HTVM.EDSL.Printer

-- | Takes C++ program and passes it through standalone formatter
prettyCpp :: Text -> IO Text
prettyCpp t = tpack <$> readCreateProcess (shell "clang-format") (tunpack t)

-- | Compile TVM model, the binary will be placed to file @fp@
compileModuleGen :: FilePath -> CppProgram -> IO ModuleGen
compileModuleGen fp (CppProgram mod code) = do
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "c++", "-", "-ltvm", "-o", fp] =<< do
    tunpack <$> prettyCpp code
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileModuleGen failed, exit code " <> show ec
    ExitSuccess -> do
      return (ModuleGen fp mod)

-- | Execute the Model generator, return the Assembly string, suitable for `compileModel`
stage :: ModuleGen -> IO Assembly
stage (ModuleGen fp mod) =
  let
    exec_fp = if isAbsolute fp then fp else "./" <> fp
  in do
  (ec,out,err) <- readProcessWithExitCode exec_fp [] ""
  hPutStr stderr err
  case ec of
    ExitFailure ec -> do
      error $ "stage failed, exit code " <> show ec
    ExitSuccess -> do
      return (Assembly mod out)

-- | Produce the model from the Assembly, see `stage`.
compileModel :: FilePath -> Assembly -> IO ModuleLib
compileModel fp asm@(Assembly mod a) = do
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "assembler", "-shared", "-fPIC", "-o", fp, "-"] a
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileModel failed, exit code " <> show ec
    ExitSuccess -> do
      return (ModuleLib fp mod)

-- | Build TVM module @modname@ from EDSL definition.
-- This function executes @g++@ compiler and @clang-format@ pretty-printer. The
-- environment should contain all the settings required for including TVM
-- headers and linking with TVM library.
--
-- In particular, consider reviewing the following variables:
--   - @PATH@ to contain paths to @g++@ and @clang-format@ binaries
--   - @C_INCLUDE_PATH@, @CPLUS_INCLUDE_PATH@ to contain path to folder with
--     TVM headers
--   - @LIBRARY_PATH@, @LD_LIBRARY_PATH@ to contain paths to folder with TVM
--     shared libraries
--
--  FIXME: Remove modulegen after usage
buildModule :: FilePath -> Module -> IO ModuleLib
buildModule fp m =
  let
    fgen = fp<>".gen"
  in do
  mgen <- compileModuleGen fgen (printModuleGen m)
  asm <- stage mgen
  compileModel fp asm

