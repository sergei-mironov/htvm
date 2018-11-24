{-# LANGUAGE OverloadedStrings #-}
module HTVM.EDSL.Build where

import Control.Monad(when)
import Data.Text(Text)
import Data.Text.IO(writeFile)
import System.FilePath(isAbsolute)
import System.Exit(ExitCode(..))
import System.Process(readCreateProcess,readProcessWithExitCode,shell)
import System.IO (hPutStr,stdout,stderr)
import Prelude hiding(writeFile)

import HTVM.Prelude
import HTVM.EDSL.Types
import HTVM.EDSL.Monad
import HTVM.EDSL.Printer

-- | Takes C++ program and passes it through standalone formatter
prettyCpp :: Text -> IO Text
prettyCpp t = tpack <$> readCreateProcess (shell "clang-format") (tunpack t)

dumpProgram :: FilePath -> ProgramSrc -> IO ()
dumpProgram fp (ProgramSrc code) = do
  writeFile fp =<< prettyCpp code

data CompileConfig = CompileConfig {
    cc_dump :: Maybe FilePath
  , cc_copy_out :: Bool
} deriving(Read,Show,Eq,Ord)

defaultConfig :: CompileConfig
defaultConfig = CompileConfig Nothing False

-- | Compile TVM program, the binary will be placed to file @fp@
compileProgram :: CompileConfig -> FilePath -> ProgramSrc -> IO ProgramBin
compileProgram cc fp src@(ProgramSrc code) = do
  case (cc_dump cc) of
    Just dfp -> do
      dumpProgram dfp src
    Nothing -> return ()
  {- traceM (tunpack code) -}
  pcode <- tunpack <$> prettyCpp code
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "c++", "-", "-ltvm", "-o", fp] pcode
  when (cc_copy_out cc) $ do
    hPutStr stderr err
    hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      fail $ "compileProgram failure\n"
          <> "Failed program was:\n\n" <> unlines (map (\(a,b) -> b <> " " <> a) ((lines pcode)`zip`[show x | x<-[1..]])) <> "\n"
          <> "Compiler error:\n\n" <> err
          <> "Compiler exit code: " <> show ec <> "\n"
    ExitSuccess -> do
      return (ProgramBin fp)


-- | Compile TVM model, the binary will be placed to file @fp@ Return
-- `ModuleGen` object that captures its filepath and module expression
compileModuleGen :: CompileConfig -> FilePath -> ModuleGenSrc -> IO ModuleGen
compileModuleGen cc fp (ModuleGenSrc mod code) = do
  ProgramBin fp <- compileProgram cc fp (ProgramSrc code)
  return (ModuleGen fp mod)

-- | Execute the Model generator, return the Assembly string, suitable for
-- `compileModel`
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
buildModule :: CompileConfig -> FilePath -> Module -> IO ModuleLib
buildModule cc fp m = do
  withTmpf "mgen" $ \fpath -> do
    mgen <- compileModuleGen cc fpath (printModuleGen m)
    asm <- stage mgen
    compileModel fp asm


printFunction :: CompileConfig -> Function -> IO Text
printFunction cc f@(Function te) = do
  withTmpf "printer" $ \f -> do
    ProgramBin prg <- compileProgram cc f (printPrinter te)
    let exec_fp = if isAbsolute prg then prg else "./" <> prg
    (ec,out,err) <- readProcessWithExitCode exec_fp [] []
    case ec of
      ExitFailure ec -> do
        error $ "compileModel failed, exit code " <> show ec
      ExitSuccess -> return (tpack out)

