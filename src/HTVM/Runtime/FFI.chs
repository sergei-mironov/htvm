-- | DLPack message wrappers to pass data to/from TVM models

{-# LANGUAGE NondecreasingIndentation #-}

module HTVM.Runtime.FFI where

import qualified Data.Array as Array

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Bits ((.&.),shiftR)
import Data.Text (Text)
import Foreign (ForeignPtr, newForeignPtr, Ptr, Storable(..), alloca,
                allocaArray, peek, plusPtr, poke, peekArray, pokeArray,
                castPtr, advancePtr, malloc, mallocArray, FunPtr(..), free,
                withForeignPtr, nullPtr, newForeignPtr_)
import Foreign.C.Types (CInt, CLong)
import Foreign.C.String (CString, withCString, peekCAString)
import System.IO.Unsafe (unsafePerformIO)

import HTVM.Prelude


-- | Errors raised by various functions from this module
data TVMError =
    TVMAllocFailed Int
  | TVMFreeFailed Int
  | TVMModLoadFailed Int String
  | TVMFuncLoadFailed Int String
  | TVMFunCallFailed Int String
  | TVMFunCallBadType Int
  | TVMCopyFailed Int String
  deriving(Show,Read,Ord,Eq)

instance Exception TVMError


#include <dlpack/dlpack.h>
#include <tvm/runtime/c_runtime_api.h>

{# enum DLDataTypeCode as TVMDataTypeCode {upcaseFirstLetter} deriving(Eq) #}
{# enum DLDeviceType as TVMDeviceType {upcaseFirstLetter} deriving(Eq) #}
{# enum TVMDeviceExtType {upcaseFirstLetter} deriving(Eq) #}
{# enum TVMTypeCode {upcaseFirstLetter} deriving(Eq) #}

instance Storable TVMTypeCode where
  sizeOf _ = {# sizeof TVMTypeCode #}
  alignment _ = {# alignof TVMTypeCode #}
  peek pc = toEnum <$> peek (castPtr pc)
  poke pc c = poke (castPtr pc) (fromEnum c)

-- | Representation of tvm_index_t
type TVMShapeIndex = {# type tvm_index_t #}

-- | Representation of device identifiers
type TVMDeviceId = Int

-- | TODO: document
data TVMContext

instance Storable TVMContext where
  sizeOf _ = {# sizeof TVMContext #}
  alignment _ = {# alignof TVMContext #}
  peek = error "peek undefined"
  poke = error "poke undefined"

-- | Representation of `DLTensor` structure which is the Tensor container
data TVMTensor_Repr

instance Storable TVMTensor_Repr where
  sizeOf _ = {# sizeof DLTensor #}
  alignment _ = {# alignof DLTensor #}
  peek = error "peek undefined"
  poke = error "poke undefined"

-- | Alias for `TVMArrayHandle`, which internally is the same as the pointer to
-- DLTensor
type TVMArrayHandle = Ptr TVMTensor_Repr

-- | Main runtime representation of Tensors
type TVMTensor = ForeignPtr TVMTensor_Repr

-- | Alias for `TVMStreamHandle`. Not supported via this FFI currently.
type TVMStreamHandle = Ptr ()

-- | TVMValue represents function argument, accepted by TVM functions.
data TVMValue

instance Storable TVMValue where
  sizeOf _ = {# sizeof TVMValue #}
  alignment _ = {# alignof TVMValue #}
  peek = error "peek undefined"
  poke = error "poke undefined"

data TVMModule_Repr

instance Storable TVMModule_Repr where
  sizeOf _ = {# sizeof TVMModuleHandle #}
  alignment _ = {# alignof TVMModuleHandle #}
  peek = error "peek is undefined for TVMModuleHandle"
  poke = error "poke is undefined for TVMModuleHandle"

-- | Alias for void* used as Module handle
type TVMModuleHandle = Ptr TVMModule_Repr

type TVMModule = ForeignPtr TVMModule_Repr

data TVMFunction_Repr

instance Storable TVMFunction_Repr where
  sizeOf _ = {# sizeof TVMFunctionHandle #}
  alignment _ = {# alignof TVMFunctionHandle #}
  peek = error "peek is undefined for TVMFunction_Repr"
  poke = error "poke is undefined for TVMFunction_Repr"

-- | Alias for void* used as Function handle
type TVMFunctionHandle = Ptr TVMFunction_Repr

type TVMFunction = ForeignPtr TVMFunction_Repr


setTensor :: TVMTensor -> Ptr TVMValue -> Ptr TVMTypeCode -> IO ()
setTensor ft pv pc = do
  withForeignPtr ft $ \pt -> do
    poke pc KArrayHandle
    {# set TVMValue.v_handle #} pv (castPtr pt)

-- setStr :: String -> Ptr TVMValue -> Ptr TVMTypeCode -> IO ()
-- setStr s pv pc = undefined

foreign import ccall unsafe "c_runtime_api.h TVMArrayAlloc"
  tvmArrayAlloc
    :: Ptr TVMShapeIndex
                     -- shape
    -> CInt           -- ndim,
    -> CInt           -- dtype_code,
    -> CInt           -- dtype_bits,
    -> CInt           -- dtype_lanes,
    -> CInt           -- device_type,
    -> CInt           -- device_id,
    -> Ptr TVMArrayHandle
                      -- DLTensor* out
    -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMArrayFree"
  tvmArrayFree :: TVMArrayHandle -> IO CInt

foreign import ccall unsafe "c_runtime_api.h &TVMArrayFree"
  tvmArrayFree_ :: FunPtr (TVMArrayHandle -> IO ())

foreign import ccall unsafe "c_runtime_api.h TVMModLoadFromFile"
  tvmModLoadFromFile :: CString -> CString -> Ptr TVMModuleHandle -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMModFree"
  tvmModFree :: TVMModuleHandle -> IO CInt

foreign import ccall unsafe "c_runtime_api.h &TVMModFree"
  tvmModFree_ :: FunPtr (TVMModuleHandle -> IO ())

foreign import ccall unsafe "c_runtime_api.h TVMModGetFunction"
  tvmModGetFunction :: TVMModuleHandle -> CString -> CInt -> Ptr TVMFunctionHandle -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMFuncFree"
  tvmFuncFree :: TVMFunctionHandle -> IO CInt

foreign import ccall unsafe "c_runtime_api.h &TVMFuncFree"
  tvmFuncFree_ :: FunPtr (TVMFunctionHandle -> IO ())

foreign import ccall unsafe "c_runtime_api.h TVMGetLastError"
  tvmGetLastError :: IO CString

foreign import ccall unsafe "c_runtime_api.h TVMFuncCall"
  tvmFuncCall :: TVMFunctionHandle -> Ptr TVMValue -> Ptr TVMTypeCode -> CInt -> Ptr TVMValue -> Ptr TVMTypeCode -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMArrayCopyFromTo"
  tvmArrayCopyFromTo :: TVMArrayHandle -> TVMArrayHandle -> TVMStreamHandle -> IO CInt

toCInt :: (Integral x) => x -> CInt
toCInt = fromInteger . toInteger

fromCInt :: (Integral x) => CInt -> x
fromCInt = fromInteger . toInteger

tensorDevice :: TVMTensor -> TVMDeviceType
tensorDevice ft = unsafePerformIO $ do
  withForeignPtr ft $ \pt -> do
    (toEnum . fromCInt) <$> {# get DLTensor->ctx.device_type #} pt

tensorNDim :: TVMTensor -> Integer
tensorNDim ft = unsafePerformIO $ do
  withForeignPtr ft $ \pt -> do
    toInteger <$> {# get DLTensor->ndim #} pt

{-
tensorNDimM :: TVMTensor -> IO Integer
tensorNDimM ft = do
  withForeignPtr ft $ \pt -> do
    toInteger <$> {# get DLTensor->ndim #} pt
-}

tensorShape :: TVMTensor -> [Integer]
tensorShape ft = unsafePerformIO $ do
  withForeignPtr ft $ \pt -> do
    map toInteger <$> do
      peekArray (fromInteger $ tensorNDim ft) =<< {# get DLTensor->shape #} pt

-- | Access raw tensor data
tensorData :: TVMTensor -> IO (Ptr ())
tensorData p = withForeignPtr p {# get DLTensor->data #}

tensorCopy :: TVMTensor -> TVMTensor -> IO ()
tensorCopy dst src = do
  withForeignPtr dst $ \pdst -> do
  withForeignPtr src $ \psrc -> do
  ret <- tvmArrayCopyFromTo psrc pdst nullPtr
  case ret of
    0 -> return ()
    e -> do
      str <- getLastError
      throwIO (TVMCopyFailed (fromCInt e) str)


-- | Return a string describing the last error issued by TVM runtime
getLastError :: IO String
getLastError = peekCAString =<< tvmGetLastError

-- | Load module named @modname@. Module will be freed when Haskell runtime
-- decide so.
loadModule :: FilePath -> IO TVMModule
loadModule modname = do
  alloca $ \pmod -> do
  withCString modname $ \cmodname -> do
  withCString "so" $ \so -> do
    r <- tvmModLoadFromFile cmodname so pmod
    case r of
      0 -> peek pmod >>= newForeignPtr tvmModFree_
      err -> throwIO =<< (TVMModLoadFailed <$> pure (fromCInt err) <*> getLastError)

-- | Load module from dynamic library @modname@ and process it with a callback @func@
-- Module will be freed on return from @func@
withModule :: FilePath -> (TVMModule -> IO b) -> IO b
withModule modname func =
  alloca $ \pmod -> do
  withCString modname $ \cmodname -> do
  withCString "so" $ \so -> do
    r <- tvmModLoadFromFile cmodname so pmod
    case r of
      0 -> do
        m <- peek pmod
        b <- func =<< (newForeignPtr_ m)
        tvmModFree m
        return b
      err -> do
        throwIO =<< (TVMModLoadFailed <$> pure (fromCInt err) <*> getLastError)

-- | Load function named @funcname@ from module @mod@. Function will be
-- freed when Haskell runtime decide so.
loadFunction :: Text -> TVMModule -> IO TVMFunction
loadFunction funcname mod = do
  alloca $ \pfunc -> do
  withCString (tunpack funcname) $ \cfuncname -> do
  withForeignPtr mod $ \hmod -> do
    r <- tvmModGetFunction hmod cfuncname 0 pfunc
    case r of
      0 -> peek pfunc >>= newForeignPtr tvmFuncFree_
      err -> throwIO =<< (TVMFuncLoadFailed <$> pure (fromCInt err) <*> getLastError)

-- | Load the function named @funcname@ from module @mod@, use it in callback @func@
-- Function will be freed on return from @func@
withFunction :: Text -> TVMModule -> (TVMFunction -> IO b) -> IO b
withFunction funcname mod func =
  alloca $ \pfunc -> do
  withCString (tunpack funcname) $ \cfuncname -> do
  withForeignPtr mod $ \hmod -> do
    r <- tvmModGetFunction hmod cfuncname 0 pfunc
    case r of
      0 -> do
        f <- peek pfunc
        b <- func =<< (newForeignPtr_ f)
        tvmFuncFree f
        return b
      err -> do
        str <- getLastError
        throwIO (TVMFuncLoadFailed (fromInteger $ toInteger err) str)

-- | Call function @fun@ returning @ret@ with @args@
--
-- TODO: Process pvretcode
callTensorFunction :: TVMTensor -> TVMFunction -> [TVMTensor] -> IO ()
callTensorFunction ret fun args =
  let
    nargs = length args + 1 {- Result is also counts -}
    clen = toCInt nargs
  in do
  alloca $ \pvret -> do
  alloca $ \pvretcode -> do
  allocaArray nargs $ \pvargs -> do
  allocaArray nargs $ \pvargcodes -> do
  withForeignPtr fun $ \hfun -> do
    forM_ ((args<>[ret])`zip`[0..nargs-1]) $ \(farg,off) -> do
      case off < length args of
        True -> setTensor farg (advancePtr pvargs off) (advancePtr pvargcodes off)
        False -> setTensor ret (advancePtr pvargs off) (advancePtr pvargcodes off)
    r <- tvmFuncCall hfun pvargs pvargcodes clen pvret pvretcode
    case r of
      0 -> do
        return ()
      x -> do
        str <- getLastError
        throwIO (TVMFunCallFailed (fromCInt x) str)

