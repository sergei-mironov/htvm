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

-- | Compile the model, obtain the generator binary
compileGen :: FilePath -> Library -> IO ()
compileGen fp lib = do
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "c++", "-", "-ltvm", "-o", fp] =<< do
    tunpack <$> prettyCpp (printProgram lib)
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileGen failed, exit code " <> show ec
    ExitSuccess -> do
      return ()

-- | Execute the generator, return the Assembly string for `compileModel`
stage :: FilePath -> IO String
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
      return out

-- | Produce the model from Assembly< see `stage`.
compileModel :: FilePath -> String -> IO ()
compileModel fp asm = do
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "assembler", "-shared", "-fPIC", "-o", fp, "-"] asm
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileModel failed, exit code " <> show ec
    ExitSuccess -> do
      return ()

-- | Build module @modname@ from EDSL library definition.
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
buildModule :: FilePath -> Library -> IO ()
buildModule fp l =
  let
    fgen = fp<>".gen"
  in do
  compileGen fgen l
  asm <- stage fgen
  compileModel fp asm

