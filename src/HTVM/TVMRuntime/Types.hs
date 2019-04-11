module HTVM.TVMRuntime.Types where

import Data.Monoid
import Data.Text(Text)

import HTVM.EDSL

data BackendType = BackendLLVM | BackendCUDA
  deriving(Show,Read,Ord,Eq)

-- | ModuleGenSrc represents C++ sources of Module generator. @mgen_ast@ is a
-- user-defined data representing some initial AST of the module
data ModuleGenSrc a = ModuleGenSrc {
    mgs_ast :: a
  , mgs_backend :: BackendType
  , mgs_src :: Text
  }
  deriving(Show,Read,Eq,Ord)

-- | Represents C++ sources of arbitrary program
data ProgramSrc = ProgramSrc { prog_src :: Text }
  deriving(Show,Read,Eq,Ord)

-- | Represent path to arbitrary program's binary
data ProgramBin = ProgramBin FilePath
  deriving(Show,Read,Eq,Ord)

-- | ModuleGen represents path to compiled TVM module generator binary. @a@
-- represents AST source of the module.
data ModuleGen a = ModuleGen {
    mg_filePath :: FilePath
  , mg_src :: ModuleGenSrc a
  } deriving(Show,Read,Eq,Ord)

-- | Assembly produced by TVM Module generator, along with source
-- expression and backend information
data Assembly a = Assembly {
    asm_src :: ModuleGenSrc a -- ^ Sources of ModuleGen which produced this assembly
  , asm_source :: String -- ^ Assembly text
  } deriving(Show,Read,Eq,Ord)

-- | Path to compiled TVM Module along with its source expression. Typically
-- the module is contained in a shared library
data ModuleLib a = ModuleLib FilePath a
  deriving(Show,Read,Eq,Ord)
