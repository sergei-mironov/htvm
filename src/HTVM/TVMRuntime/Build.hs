{-# LANGUAGE OverloadedStrings #-}
module HTVM.TVMRuntime.Build where

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
import HTVM.TVMRuntime.Types
import HTVM.TVMRuntime.PrinterCPP

import qualified Data.Text as Text

-- | Takes C++ program and passes it through standalone formatter
prettyCpp :: Text -> IO Text
prettyCpp t = tpack <$> readCreateProcess (shell "clang-format") (tunpack t)

-- | Dump program sources to text file @fp@
dumpProgram :: FilePath -> ProgramSrc -> IO ()
dumpProgram fp (ProgramSrc code) = do
  writeFile fp =<< prettyCpp code

data CompileConfig = CompileConfig {
    -- ^ Optionally, dump the program
    cc_dump :: Maybe FilePath
    -- ^ By default program will be printed to stdout in case of failure. Set to
    -- True to always print the program
  , cc_copy_module_compiler_output :: Bool
} deriving(Read,Show,Eq,Ord)

defaultConfig :: CompileConfig
defaultConfig = CompileConfig Nothing False

defaultBackend :: BackendType
defaultBackend = BackendLLVM

-- | Prepends line numbers to the text
withLineNumbers :: Text -> Text
withLineNumbers code = Text.unlines (map (\(a,b) -> b <> " " <> a) ((Text.lines code)`zip`[tshow x | x<-[1..]]))

-- | Compile some program, the binary will be placed to file @fp@
compileProgram :: CompileConfig -> FilePath -> ProgramSrc -> IO ProgramBin
compileProgram cc fp src@(ProgramSrc code) = do
  case (cc_dump cc) of
    Just dfp -> do
      dumpProgram dfp src
    Nothing -> return ()
  {- traceM (tunpack code) -}
  pcode <- tunpack <$> prettyCpp code
  (ec,out,err) <- readProcessWithExitCode "g++" ["-std=c++14", "-x", "c++", "-", "-ltvm", "-o", fp] pcode
  when (cc_copy_module_compiler_output cc) $ do
    hPutStr stderr err
    hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      fail $ tunpack $
             "compileProgram failure\n"
          <> "Failed program was:\n\n"
          <> withLineNumbers code <> "\n"
          <> "Compiler error:\n\n" <> tpack err
          <> "Compiler exit code: " <> tshow ec <> "\n"
    ExitSuccess -> do
      return (ProgramBin fp)

-- | Compile TVM model, the binary will be placed to file @fp@ Return
-- `ModuleGen` object that captures its filepath and module expression
compileModuleGen :: CompileConfig -> FilePath -> ModuleGenSrc a -> IO (ModuleGen a)
compileModuleGen cc fp mgs@(ModuleGenSrc mod backend_type code) = do
  ProgramBin fp <- compileProgram cc fp (ProgramSrc code)
  return (ModuleGen fp backend_type mgs)

-- | Execute the Module generator, return the Assembly string, suitable for
-- `compileModule`
runModuleGen :: ModuleGen a -> IO (Assembly a)
runModuleGen (ModuleGen fp _ mgs) =
  let
    exec_fp = if isAbsolute fp then fp else "./" <> fp
  in do
  (ec,out,err) <- readProcessWithExitCode exec_fp [] ""
  case ec of
    ExitFailure ec -> do
      fail $ tunpack $
              "runModuleGen failure.\n"
           <> "ModuleGen source code is: \n\n"
           <> withLineNumbers (mgs_src mgs) <> "\n"
           <> "ModuleGen error message:\n\n"
           <> tpack err
           <> "ModuleGen exit code: " <> tshow ec
    ExitSuccess -> do
      return (Assembly mgs out)

-- | Produce the model library from the Assembly, see `runModuleGen`.
-- Binary will be placed to output file @fp@
compileModule :: FilePath -> Assembly a -> IO (ModuleLib a)
compileModule fp asm@(Assembly mgs asm_source) = do
  (ec,out,err) <-
    readProcessWithExitCode
      "g++" ["-std=c++14", "-x", "assembler", "-shared", "-fPIC", "-o", fp, "-"]
      asm_source
  hPutStr stderr err
  hPutStr stdout out
  case ec of
    ExitFailure ec -> do
      error $ "compileModule: g++ failed, exit code " <> show ec
    ExitSuccess -> do
      return (ModuleLib fp (mgs_ast mgs))

