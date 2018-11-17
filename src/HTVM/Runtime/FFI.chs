-- | Module defines wrappers for DLPack messages which are used by TVM to pass
-- to/from models

-- {-# OPTIONS_GHC -fwarn-unused-imports #-}
-- {-# OPTIONS_GHC -fwarn-missing-signatures #-}
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
                withForeignPtr)
import Foreign.C.Types (CInt, CLong)
import Foreign.C.String (CString, withCString, peekCAString)
import System.IO.Unsafe (unsafePerformIO)

import HTVM.Prelude


data TVMError =
    TVMAllocFailed Int
  | TVMFreeFailed Int
  | TVMModLoadFailed Int String
  | TVMFuncLoadFailed Int String
  | TVMFunCallFailed Int
  | TVMFunCallBadType Int
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

type TVMShapeIndex = {# type tvm_index_t #}
type TVMDeviceId = Int

data TVMContext

instance Storable TVMContext where
  sizeOf _ = {# sizeof TVMContext #}
  alignment _ = {# alignof TVMContext #}
  peek = error "peek undefined"
  poke = error "poke undefined"

-- | Tensor representation, see `DLTensor`
data TVMTensor_Repr

instance Storable TVMTensor_Repr where
  sizeOf _ = {# sizeof DLTensor #}
  alignment _ = {# alignof DLTensor #}
  peek = error "peek undefined"
  poke = error "poke undefined"

-- | Alias for `TVMArrayHandle`
type TVMArrayHandle = Ptr TVMTensor_Repr
type TVMTensor = ForeignPtr TVMTensor_Repr



data TVMValue

instance Storable TVMValue where
  sizeOf _ = {# sizeof TVMValue #}
  alignment _ = {# alignof TVMValue #}
  peek = error "peek undefined"
  poke = error "poke undefined"

type TVMModule = Ptr ()
type TVMFunction = Ptr ()

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

instance TVMElemType Int32 where tvmTypeCode = KDLInt; tvmTypeBits = 32; tvmTypeLanes = 1
instance TVMElemType Word32 where tvmTypeCode = KDLUInt; tvmTypeBits = 32; tvmTypeLanes = 1
instance TVMElemType Float where tvmTypeCode = KDLFloat; tvmTypeBits = 32; tvmTypeLanes = 1
instance TVMElemType Int64 where tvmTypeCode = KDLUInt; tvmTypeBits = 64; tvmTypeLanes = 1
instance TVMElemType Word64 where tvmTypeCode = KDLUInt; tvmTypeBits = 64; tvmTypeLanes = 1
instance TVMElemType Double where tvmTypeCode = KDLFloat; tvmTypeBits = 64; tvmTypeLanes = 1

-- | Data source. @d@ is type of data, @i@ is a type of index, @e@ is a type of element
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
instance TVMData [Int32] Integer Int32 where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Word32] Integer Word32 where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Float] Integer Float where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Int64] Integer Int64 where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
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

-- | FIXME: non-CPU devices will not work, see FIXME in `pokeTensor`
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

{-
-- deprecated
withTensorInput :: forall d i e b . (TVMData d i e)
  => d                           -- ^ TvmData tensor-like object
  -> TVMDeviceType               -- ^ Device type
  -> TVMDeviceId                 -- ^ Device ID
  -> (Ptr TVMTensor -> IO b)     -- ^ Handler funtion
  -> IO b
withTensorInput d dt did f = do
  alloca $ \ptensor ->
    let
      shape = map fromInteger $ tvmDataShape d
      ndim = fromInteger $ tvmDataDims d
    in
    allocaArray ndim $ \pshape -> do
      pokeArray pshape shape
      r <- tvmArrayAlloc
              pshape (toCInt ndim)
              (toCInt $ fromEnum $ tvmTypeCode @e)
              (fromInteger $ tvmTypeBits @e)
              (fromInteger $ tvmTypeLanes @e)
              (toCInt $ fromEnum dt)
              (toCInt $ did)
              ptensor
      case r of
        0 -> do
          {- Copying data from TVMData d-}
          pdata <- {# get DLTensor->data #} ptensor
          tvmPoke d (castPtr pdata)
          {- Calling user handler -}
          b <- f ptensor
          r2 <- tvmArrayFree ptensor
          case r2 of
            0 -> return b
            e -> throwIO (TVMFreeFailed (fromCInt e))
        e -> throwIO (TVMAllocFailed (fromCInt e))

withTensorOutput :: forall d i e b . (TVMData d i e)
  => [Integer]
  -> TVMDeviceType
  -> TVMDeviceId
  -> (Ptr TVMTensor -> IO b)
  -> IO (d,b)
withTensorOutput shape dt did f = do
  alloca $ \ptensor ->
    let
      ndim = length shape
    in
    allocaArray ndim $ \pshape -> do
      pokeArray pshape (map (fromInteger . toInteger) shape)
      r <- tvmArrayAlloc
              pshape (toCInt ndim)
              (toCInt $ fromEnum $ tvmTypeCode @e)
              (fromInteger $ tvmTypeBits @e)
              (fromInteger $ tvmTypeLanes @e)
              (toCInt $ fromEnum dt)
              (toCInt $ did)
              ptensor
      case r of
        0 -> do
          {- Calling user handler -}
          b <- f ptensor
          {- Copying data from TVMData d-}
          pdata <- {# get DLTensor->data #} ptensor
          d <- tvmPeek shape (castPtr pdata)
          r <- tvmArrayFree ptensor
          case r of
            0 -> return (d,b)
            e -> throwIO (TVMFreeFailed $ fromInteger $ toInteger e)
        e -> throwIO (TVMAllocFailed $ fromCInt e)
-}

foreign import ccall unsafe "c_runtime_api.h TVMModLoadFromFile"
  tvmModLoadFromFile :: CString -> CString -> Ptr TVMModule -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMModGetFunction"
  tvmModGetFunction :: TVMModule -> CString -> CInt -> Ptr TVMFunction -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMGetLastError"
  tvmGetLastError :: IO CString

foreign import ccall unsafe "c_runtime_api.h TVMFuncCall"
  tvmFuncCall :: TVMFunction -> Ptr TVMValue -> Ptr TVMTypeCode -> CInt -> Ptr TVMValue -> Ptr TVMTypeCode -> IO CInt

getLastError :: IO String
getLastError = peekCAString =<< tvmGetLastError

-- | Load module from 'so' dynamic library
-- TODO: Unload the module
-- TODO: Pass GetLastError in case of failure
withModule :: Text -> (TVMModule -> IO b) -> IO b
withModule modname func =
  alloca $ \pmod -> do
  withCString (tunpack modname) $ \cmodname -> do
  withCString "so" $ \so -> do
    r <- tvmModLoadFromFile cmodname so pmod
    case r of
      0 -> func =<< peek pmod
      err -> do
        str <- getLastError
        throwIO (TVMModLoadFailed (fromInteger $ toInteger err) str)

-- | Load the function from module
-- TODO: Unload the module
-- TODO: Pass GetLastError in case of failure
withFunction :: Text -> TVMModule -> (TVMFunction -> IO b) -> IO b
withFunction funcname mod func =
  alloca $ \pfunc -> do
  withCString (tunpack funcname) $ \cfuncname -> do
    r <- tvmModGetFunction mod cfuncname 0 pfunc
    case r of
      0 -> func =<< peek pfunc
      err -> do
        str <- getLastError
        throwIO (TVMFuncLoadFailed (fromInteger $ toInteger err) str)

{-
callTensorFunction :: forall d i e . (TVMData d i e) => [Integer] -> TVMFunction -> [d] -> IO d
callTensorFunction oshape fun ts0 =
  let
    devtype = KDLCPU
    devid = 0
    go pts (t:ts) = withTensorInput t devtype devid $ \pt -> go (pt:pts) ts
    go pts [] = withTensorOutput oshape devtype devid $ \pto -> do
      alloca $ \pret -> do
      alloca $ \pretcode -> do
      allocaArray (length pts) $ \pv -> do
      allocaArray (length pts) $ \pc -> do
        forM_ (pts`zip`[0..(length pts)-1]) $ \(pt,off) -> do
          setTensor pt (advancePtr pv off) (advancePtr pc off)
        setTensor pto pret pretcode
        let clen = fromInteger $ toInteger $ length pts
        r <- tvmFuncCall fun pv pc clen pret pretcode
        rt <- peek pretcode
        case (r,rt) of
          (0,KArrayHandle) -> do
            return ()
          (x,KArrayHandle) -> do
            throwIO (TVMFunCallFailed $ fromInteger $ toInteger x)
          (0,t) -> do
            throwIO (TVMFunCallBadType $ fromEnum rt)
          _ -> error "callTensorFunction: unexpected return code"
  in
  fst <$> go [] ts0
-}

{-
TVM_DLL int TVMModGetFunction(TVMModuleHandle mod,
                              const char* func_name,
                              int query_imports,
                              TVMFunctionHandle *out);
-}

{-
foreign import ccall unsafe "dlfcn.h dlopen"
  dlopen :: CString -> Int -> IO (Ptr ModuleHandle)
foreign import ccall unsafe "dlfcn.h dlclose"
  dlclose :: Ptr ModuleHandle -> IO ()
-}



{-

{#
enum mtl_Formula_type as Mtl_Formula_Type {upcaseFirstLetter}
    deriving (Eq, Show)
#}



instance Storable ??? where
    sizeOf _ = {# sizeof mtl_State #} + maxSubformulas * {# sizeof mtl_Payload #}
    alignment _ = {# alignof mtl_Formula #}
    peek = error ("peek is not implemented for the State datatype")
    poke mptr ??? = do
        let sf = unfold f
        when (length sf > maxSubformulas) $
            fail $ "Subformulas number limit (" ++ show maxSubformulas ++ ") is reached"
        let ppl num =
                mptr
                `plusPtr` {# sizeof mtl_State #}
                `plusPtr` (num * {# sizeof mtl_Formula_subf #})
        {# set mtl_State.pl #} mptr (ppl 0)
        {# set mtl_State.pl_size #} mptr (fromIntegral $ length sf)

-- | Represents the immutable part of a formula on the C-side
instance Storable ??? where
    sizeOf _ = {# sizeof mtl_Formula #}
    alignment _ = {# alignof mtl_Formula #}
    peek = error ("peek is not implemented for the Formula datatype")
    poke iptr ??? = do
        let sf = unfold f
        when (length sf > maxSubformulas) $
            fail $ "Subformulas number limit (" ++ show maxSubformulas ++ ") is reached"

        let -- | Returns pointer to subformula @n@, the array os stored after mtl_Formula
            psubf num =
                iptr
                `plusPtr` {# sizeof mtl_Formula #}
                `plusPtr` (num * {# sizeof mtl_Formula_subf #})

        {# set mtl_Formula.subf #} iptr (psubf 0)
        {# set mtl_Formula.subf_size #} iptr (fromIntegral $ length sf)
        for_ (sf `zip` [0 ..]) $ \(f', i) -> do
            {# set mtl_Formula_subf.t #} (psubf i) (ft2ct (ftype f'))
            {# set mtl_Formula_subf.argn #} (psubf i) (fromIntegral . fromMaybe (-1) $ argn f')
            {# set mtl_Formula_subf.nm.pname #} (psubf i)
                (case predA f' of
                    Just (PName nm1) -> fromIntegral $ hash nm1
                    Nothing -> -1
                )
            {# set mtl_Formula_subf.p1.pos #} (psubf i) (fromIntegral $ fromMaybe (-1) $ (fst <$> snd <$> payload <$> subfn 0 f'))
            {# set mtl_Formula_subf.p2.pos #} (psubf i) (fromIntegral $ fromMaybe (-1) $ (fst <$> snd <$> payload <$> subfn 1 f'))

-}
