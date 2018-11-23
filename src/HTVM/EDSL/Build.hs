{-# LANGUAGE OverloadedStrings #-}
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

-- | Compile TVM program, the binary will be placed to file @fp@
compileProgram :: FilePath -> ProgramSrc -> IO ProgramBin
compileProgram fp (ProgramSrc code) = do
  {- traceM (tunpack code) -}
  pcode <- tunpack <$> prettyCpp code
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "c++", "-", "-ltvm", "-o", fp] pcode
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      fail $ "compileProgram failed, exit code " <> show ec <> "\nFailed program was:\n" <> unlines (map (\(a,b) -> b <> " " <> a) ((lines pcode)`zip`[show x | x<-[1..]]))
    ExitSuccess -> do
      return (ProgramBin fp)


-- | Compile TVM model, the binary will be placed to file @fp@ Return
-- `ModuleGen` object that captures its filepath and module expression
compileModuleGen :: FilePath -> ModuleGenSrc -> IO ModuleGen
compileModuleGen fp (ModuleGenSrc mod code) = do
  ProgramBin fp <- compileProgram fp (ProgramSrc code)
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

-- | Produce the model library from the Assembly, see `stage`.
-- Binary will be placed to output file @fp@
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
buildModule :: FilePath -> Module -> IO ModuleLib
buildModule fp m = do
  withTmpf "mgen" $ \fpath -> do
    mgen <- compileModuleGen fpath (printModuleGen m)
    asm <- stage mgen
    compileModel fp asm


printFunction :: Function -> IO Text
printFunction f@(Function te) = do
  withTmpf "printer" $ \f -> do
    ProgramBin prg <- compileProgram f (printPrinter te)
    let exec_fp = if isAbsolute prg then prg else "./" <> prg
    (ec,out,err) <- readProcessWithExitCode exec_fp [] []
    case ec of
      ExitFailure ec -> do
        error $ "compileModel failed, exit code " <> show ec
      ExitSuccess -> return (tpack out)

