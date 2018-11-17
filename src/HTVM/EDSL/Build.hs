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
compileGen :: FilePath -> CppProgram -> IO ()
compileGen fp (CppProgram code) = do
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "c++", "-", "-ltvm", "-o", fp] =<< do
    tunpack <$> prettyCpp code
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileGen failed, exit code " <> show ec
    ExitSuccess -> do
      return ()

-- | Execute the Model generator, return the Assembly string, suitable for `compileModel`
stage :: FilePath -> IO Assembly
stage fp =
  let
    exec_fp = if isAbsolute fp then fp else "./" <> fp
  in do
  (ec,out,err) <- readProcessWithExitCode exec_fp [] ""
  hPutStr stderr err
  case ec of
    ExitFailure ec -> do
      error $ "stage failed, exit code " <> show ec
    ExitSuccess -> do
      return (Assembly out)

-- | Produce the model from the Assembly, see `stage`.
compileModel :: FilePath -> Assembly -> IO ()
compileModel fp (Assembly asm) = do
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "assembler", "-shared", "-fPIC", "-o", fp, "-"] asm
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileModel failed, exit code " <> show ec
    ExitSuccess -> do
      return ()

-- | Build TVM module @modname@ from EDSL definition.
-- This function executes @g++@ compiler and @clang-format@ prettifier which
-- should present in @PATH@. The environment should contain all the settings
-- required for including TVM headers and linking with TVM library.
--
-- In particular, consider reviewing the following variables:
--   - @PATH@ to contain paths to @g++@ and @clang-format@ binaries
--   - @C_INCLUDE_PATH@, @CPLUS_INCLUDE_PATH@ to contain path to folder with
--     TVM headers
--   - @LIBRARY_PATH@, @LD_LIBRARY_PATH@ to contain paths to folder with TVM
--     shared libraries
buildModule :: FilePath -> Module -> IO ()
buildModule fp m =
  let
    fgen = fp<>".gen"
  in do
  compileGen fgen (printModuleGen m)
  asm <- stage fgen
  compileModel fp asm

