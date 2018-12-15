-- | DLPack message wrappers to pass data to/from TVM models

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}

module HTVM.Runtime.FFI where

import qualified Data.Array as Array

import Control.Exception (Exception, throwIO)
import Control.Arrow ((***))
import Control.Monad (forM_)
import Data.Array (Array(..))
import Data.ByteString (ByteString,pack)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Bits (FiniteBits(..),(.&.),shiftR)
import Data.Tuple (swap)
import Data.Text (Text)
import Data.List (nub)
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

class TVMIndex i where
  tvmList :: i -> [Integer]

instance TVMIndex Integer where tvmList a = [a]
instance TVMIndex (Integer,Integer) where tvmList (a,b) = [a,b]
instance TVMIndex (Integer,Integer,Integer) where tvmList (a,b,c) = [a,b,c]
instance TVMIndex (Integer,Integer,Integer,Integer) where tvmList (a,b,c,d) = [a,b,c,d]

tvmIndexDims :: (TVMIndex i) => i -> Integer
tvmIndexDims = ilength . tvmList

class TVMElemType e where
  tvmTypeCode :: TVMDataTypeCode
  tvmTypeBits :: Integer
  -- | Make a parameter of type
  tvmTypeLanes :: Integer

instance TVMElemType Int32  where tvmTypeCode = KDLInt; tvmTypeBits = 32; tvmTypeLanes = 1
instance TVMElemType Word32 where tvmTypeCode = KDLUInt; tvmTypeBits = 32; tvmTypeLanes = 1
instance TVMElemType Float  where tvmTypeCode = KDLFloat; tvmTypeBits = 32; tvmTypeLanes = 1
instance TVMElemType Int64  where tvmTypeCode = KDLUInt; tvmTypeBits = 64; tvmTypeLanes = 1
instance TVMElemType Word64 where tvmTypeCode = KDLUInt; tvmTypeBits = 64; tvmTypeLanes = 1
instance TVMElemType Double where tvmTypeCode = KDLFloat; tvmTypeBits = 64; tvmTypeLanes = 1

-- | Data bundle accepted by TVM model. @d@ is type of data, @i@ is a type of
-- index, @e@ is the type of element
class (TVMIndex i, TVMElemType e) => TVMData d i e | d -> i, d -> e where
  tvmIShape :: d -> [Integer]
  tvmIndex :: d -> i -> IO e
  tvmPeek :: [Integer] -> Ptr e -> IO d
  tvmPoke :: d -> Ptr e -> IO ()
  -- ^ Write the contents of data to dense memory area.
  -- TODO: figure out the alignment restirctions.

instance (Storable e, Array.Ix i, TVMIndex i, TVMElemType e) => TVMData (Array i e) i e where
  tvmIShape = map (uncurry (-)) . uncurry zip . (tvmList *** tvmList) . Array.bounds
  tvmIndex d i = pure $ d Array.! i
  tvmPoke d ptr = pokeArray ptr (Array.elems d)
  tvmPeek shape ptr = error "peek is undefined for Arrays"

tvmIShape1 d = [ilength d]
tvmIndex1 l i = pure $ l !! (fromInteger i)
tvmPoke1 d ptr = pokeArray ptr d
tvmPeek1 [x] ptr = peekArray (fromInteger x) ptr
tvmPeek1 _ ptr = error "tvmPeek1 should be called with single-element shape"
instance TVMData [Int32] Integer Int32   where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Word32] Integer Word32 where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Float] Integer Float   where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Int64] Integer Int64   where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Word64] Integer Word64 where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Double] Integer Double where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1

tvmIShape2 [] = [0,0]
tvmIShape2 d = [ilength d, ilength (head d)]
tvmIndex2 l (r,c) = pure $ l !! (fromInteger r) !! (fromInteger c)
tvmPoke2 d ptr
  | length d == 0 = pokeArray ptr (concat d)
  | length (nub (map length d)) == 1 = pokeArray ptr (concat d)
  | otherwise = error "All elements should have the same length"
tvmPeek2 [0,0] ptr = pure []
tvmPeek2 [x,y] ptr = group y <$>  peekArray (fromInteger $ x*y) ptr where
  group :: Integer -> [a] -> [[a]]
  group _ [] = []
  group n l
    | n > 0 = (take (fromInteger n) l) : (group n (drop (fromInteger n) l))
    | otherwise = error "Negative n"
tvmPeek2 x _ = error "tvmPeek2 should be called with 2-element shape"
instance TVMData [[Int32]] (Integer,Integer) Int32 where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmPoke = tvmPoke2; tvmPeek = tvmPeek2
instance TVMData [[Word32]] (Integer,Integer) Word32 where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmPoke = tvmPoke2; tvmPeek = tvmPeek2
instance TVMData [[Float]] (Integer,Integer) Float where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmPoke = tvmPoke2; tvmPeek = tvmPeek2
instance TVMData [[Int64]] (Integer,Integer) Int64 where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmPoke = tvmPoke2; tvmPeek = tvmPeek2
instance TVMData [[Word64]] (Integer,Integer) Word64 where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmPoke = tvmPoke2; tvmPeek = tvmPeek2
instance TVMData [[Double]] (Integer,Integer) Double where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmPoke = tvmPoke2; tvmPeek = tvmPeek2

tvmDataShape :: (TVMData d i e) => d -> [Integer]
tvmDataShape = tvmIShape

tvmDataNDim :: (TVMData d i e) => d -> Integer
tvmDataNDim = ilength . tvmDataShape

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

-- | Create new empty TVMTensor object. This object will be managed by Haskell
-- runtime which would call free when it decides to do so.
--
-- FIXME: non-CPU devices will not work, see FIXME in `pokeTensor`
newEmptyTensor :: forall e . (TVMElemType e)
  => [Integer]                   -- ^ Shape
  -> TVMDeviceType               -- ^ Device type (CPU|GPU|etc)
  -> TVMDeviceId                 -- ^ Device ID
  -> IO TVMTensor
newEmptyTensor shape dt did =
  let
    ndim = length shape
  in do
  alloca $ \pt -> do
  allocaArray ndim $ \pshape -> do
    pokeArray pshape (map (fromInteger . toInteger) shape)
    r <-
      tvmArrayAlloc
         pshape
         (fromInteger $ toInteger $ ndim)
         (toCInt $ fromEnum $ tvmTypeCode @e)
         (toCInt $ tvmTypeBits @e)
         (toCInt $ tvmTypeLanes @e)
         (toCInt $ fromEnum dt)
         (toCInt $ did) pt
    case r of
      0 -> peek pt >>= newForeignPtr tvmArrayFree_
      e -> throwIO (TVMAllocFailed (fromCInt e))

-- | Allocate new Tensor object
newTensor :: forall d i e . (TVMData d i e)
  => d                           -- ^ TvmData tensor-like object
  -> TVMDeviceType               -- ^ Device type
  -> TVMDeviceId                 -- ^ Device ID
  -> IO TVMTensor
newTensor d dt did = do
  ft <- newEmptyTensor @e (map fromInteger $ tvmDataShape d) dt did
  withForeignPtr ft $ \pt -> do
    pdata <- {# get DLTensor->data #} pt
    tvmPoke d (castPtr pdata)
  return ft


peekTensor :: forall d i e b . (TVMData d i e)
  => TVMTensor -> IO d
peekTensor ft = do
  withForeignPtr ft $ \pt -> do
    case tensorDevice ft of
      KDLCPU -> do
        pdata <- {# get DLTensor->data #} pt
        tvmPeek (tensorShape ft) (castPtr pdata)
      x -> do
        fail "Not implemented"

pokeTensor :: forall d i e b . (TVMData d i e)
  => TVMTensor -> d -> IO ()
pokeTensor ft d = do
  withForeignPtr ft $ \pt -> do
    case tensorDevice ft of
      KDLCPU -> do
        pdata <- {# get DLTensor->data #} pt
        {- FIXME: Use CopyFromBytes for non-CPU devices -}
        tvmPoke d (castPtr pdata)
      x -> do
        fail "Not implemented"


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

