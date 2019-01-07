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
compileModuleGen :: CompileConfig -> FilePath -> ModuleGenSrc a -> IO (ModuleGen a)
compileModuleGen cc fp (ModuleGenSrc mod code) = do
  ProgramBin fp <- compileProgram cc fp (ProgramSrc code)
  return (ModuleGen fp mod)

-- | Execute the Module generator, return the Assembly string, suitable for
-- `compileModule`
runModuleGen :: ModuleGen a -> IO (Assembly a)
runModuleGen (ModuleGen fp mod) =
  let
    exec_fp = if isAbsolute fp then fp else "./" <> fp
  in do
  (ec,out,err) <- readProcessWithExitCode exec_fp [] ""
  hPutStr stderr err
  case ec of
    ExitFailure ec -> do
      error $ "runModuleGen: model generator failed, exit code " <> show ec
    ExitSuccess -> do
      return (Assembly mod out)

-- | Produce the model library from the Assembly, see `runModuleGen`.
-- Binary will be placed to output file @fp@
compileModule :: FilePath -> Assembly a -> IO (ModuleLib a)
compileModule fp asm@(Assembly mod a) = do
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "assembler", "-shared", "-fPIC", "-o", fp, "-"] a
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileModule: g++ failed, exit code " <> show ec
    ExitSuccess -> do
      return (ModuleLib fp mod)

