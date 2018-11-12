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


module HTVM.Runtime.FFI where

import qualified Data.Array as Array

import Control.Exception (Exception, throwIO)
import Control.Arrow ((***))
import Data.Array (Array(..))
import Data.ByteString (ByteString,pack)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Bits (FiniteBits(..),(.&.),shiftR)
import Data.Tuple (swap)
import Data.Text (Text)
import Foreign (Ptr, Storable(..), alloca, allocaArray, peek, plusPtr, poke, pokeArray, castPtr)
import Foreign.C.Types (CInt, CLong)
import Foreign.C.String (CString, withCString, peekCAString)
import System.IO.Unsafe (unsafePerformIO)

import HTVM.Prelude


data TVMError =
    TVMAllocFailed Int
  | TVMFreeFailed Int
  | TVMModLoadFailed Int String
  | TVMFuncLoadFailed Int
  deriving(Show,Read,Ord,Eq)

instance Exception TVMError


#include <dlpack/dlpack.h>
#include <tvm/runtime/c_runtime_api.h>

{# enum DLDataTypeCode as TVMDataTypeCode {upcaseFirstLetter} deriving(Eq) #}
{# enum DLDeviceType as TVMDeviceType {upcaseFirstLetter} deriving(Eq) #}

{# enum TVMDeviceExtType {upcaseFirstLetter} deriving(Eq) #}
{# enum TVMTypeCode {upcaseFirstLetter} deriving(Eq) #}

type TVMShapeIndex = {# type tvm_index_t #}
type TVMDeviceId = Int

data TVMContext
data TVMTensor

instance Storable TVMTensor where
  sizeOf _ = {# sizeof DLTensor #}
  alignment _ = {# alignof DLTensor #}
  peek = undefined
  poke = undefined

foreign import ccall unsafe "c_runtime_api.h TVMArrayAlloc"
  tvmArrayAlloc
    :: Ptr TVMShapeIndex
                     -- shape
    -> Int           -- ndim,
    -> Int           -- dtype_code,
    -> Int           -- dtype_bits,
    -> Int           -- dtype_lanes,
    -> Int           -- device_type,
    -> Int           -- device_id,
    -> Ptr TVMTensor -- DLTensor* out
    -> IO Int

foreign import ccall unsafe "c_runtime_api.h TVMArrayFree"
  tvmArrayFree :: Ptr TVMTensor -> IO Int



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
instance TVMElemType Float where tvmTypeCode = KDLFloat; tvmTypeBits = 32; tvmTypeLanes = 1
instance TVMElemType Word64 where tvmTypeCode = KDLUInt; tvmTypeBits = 64; tvmTypeLanes = 1

class (TVMIndex i, TVMElemType e) => TVMData d i e | d -> i, d -> e where
  tvmIShape :: d -> [Integer]
  tvmIndex :: d -> i -> IO e
  tvmFill :: d -> Ptr e -> IO ()

instance (Storable e, Array.Ix i, TVMIndex i, TVMElemType e) => TVMData (Array i e) i e where
  tvmIShape = map (uncurry (-)) . uncurry zip . (tvmList *** tvmList) . Array.bounds
  tvmIndex d i = pure $ d Array.! i
  tvmFill d ptr = pokeArray ptr (Array.elems d)

tvmIShape1 d = [ilength d]
tvmIndex1 l i = pure $ l !! (fromInteger i)
tvmFill1 d ptr = pokeArray ptr d
instance TVMData [Float] Integer Float where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmFill = tvmFill1
instance TVMData [Int32] Integer Int32 where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmFill = tvmFill1
instance TVMData [Word64] Integer Word64 where tvmIShape = tvmIShape1 ; tvmIndex = tvmIndex1; tvmFill = tvmFill1

tvmIShape2 d = [ilength d, ilength (head d)]
tvmIndex2 l (r,c) = pure $ l !! (fromInteger r) !! (fromInteger c)
tvmFill2 d ptr = pokeArray ptr (concat d)
instance TVMData [[Float]] (Integer,Integer) Float where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmFill = tvmFill2
instance TVMData [[Int32]] (Integer,Integer) Int32 where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmFill = tvmFill2
instance TVMData [[Word64]] (Integer,Integer) Word64 where tvmIShape = tvmIShape2 ; tvmIndex = tvmIndex2; tvmFill = tvmFill2

tvmDataShape :: (TVMData d i e) => d -> [Integer]
tvmDataShape = tvmIShape

tvmDataDims :: (TVMData d i e) => d -> Integer
tvmDataDims = ilength . tvmDataShape

--tvmDataTypeCode :: forall d i e . (TVMData d i e) => d -> TVMDataTypeCode
--tvmDataTypeCode _ = tvmTypeCode (Proxy :: Proxy e)


with_tvmTensor :: forall d i e b . (TVMData d i e)
              => d
              -> TVMDeviceType
              -> TVMDeviceId
              -> (Ptr TVMTensor -> IO b)
              -> IO b
with_tvmTensor d dt did f = do
  alloca $ \ptensor ->
    let
      shape = map fromInteger $ tvmDataShape d
      ndim = fromInteger $ tvmDataDims d
      nels = foldr1 (*) shape
    in
    allocaArray ndim $ \pshape -> do
      pokeArray pshape shape
      r <- tvmArrayAlloc
              pshape ndim
              (fromEnum $ tvmTypeCode @e)
              (fromInteger $ tvmTypeBits @e)
              (fromInteger $ tvmTypeLanes @e)
              (fromEnum dt)
              did
              ptensor
      case r of
        0 -> do
          {- Copying data from TVMData d-}
          pdata <- {# get DLTensor->data #} ptensor
          tvmFill d (castPtr pdata)
          {- Calling user handler -}
          b <- f ptensor
          r <- tvmArrayFree ptensor
          case r of
            0 -> return b
            e -> throwIO (TVMFreeFailed e)
        e -> throwIO (TVMAllocFailed e)


type TVMModule = Ptr ()
type TVMFunction = Ptr ()

foreign import ccall unsafe "c_runtime_api.h TVMModLoadFromFile"
  tvmModLoadFromFile :: CString -> CString -> Ptr TVMModule -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMModGetFunction"
  tvmModGetFunction :: Ptr TVMModule -> CString -> CInt -> Ptr TVMFunction -> IO CInt

foreign import ccall unsafe "c_runtime_api.h TVMGetLastError"
  tvmGetLastError :: IO CString

-- | Load module from 'so' dynamic library
-- TODO: Unload the module
-- TODO: Pass GetLastError in case of failure
withModule :: Text -> (Ptr TVMModule -> IO b) -> IO b
withModule modname func =
  alloca $ \pmod -> do
  withCString (tunpack modname) $ \cmodname -> do
    r <- tvmModLoadFromFile cmodname cmodname pmod
    case r of
      0 -> func pmod
      err -> do
        str <- peekCAString =<< tvmGetLastError
        throwIO (TVMModLoadFailed (fromInteger $ toInteger err) str)

-- | Load the function from module
-- TODO: Unload the module
-- TODO: Pass GetLastError in case of failure
withFunction :: Text -> Ptr TVMModule -> (Ptr TVMFunction -> IO b) -> IO b
withFunction funcname pmod func =
  alloca $ \pfunc -> do
  withCString (tunpack funcname) $ \cfuncname -> do
    r <- tvmModGetFunction pmod cfuncname 0 pfunc
    case r of
      0 -> func pfunc
      err -> throwIO (TVMFuncLoadFailed (fromInteger $ toInteger err))

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
