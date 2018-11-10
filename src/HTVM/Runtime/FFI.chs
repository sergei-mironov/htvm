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

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString,pack)
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Bits (FiniteBits(..),(.&.),shiftR)
import Foreign (Ptr, Storable(..), alloca, allocaArray, peek, plusPtr, poke, pokeArray)
import Foreign.C.Types (CInt, CLong)
import System.IO.Unsafe (unsafePerformIO)

import HTVM.Prelude


data TVMError =
    TVMAllocFailed Int
  | TVMFreeFailed Int
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
  tvmIShape :: d -> i
  tvmIndex :: d -> i -> IO e

instance (TVMIndex i, TVMElemType e) => TVMData (Array i e) where
  tvmIShape d = Array.bounds d

tvmDataShape :: (TVMData d i e) => d -> [Integer]
tvmDataShape = tvmList . tvmIShape

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
          b <- f ptensor
          r <- tvmArrayFree ptensor
          case r of
            0 -> return b
            e -> throwIO (TVMFreeFailed e)
        e -> throwIO (TVMAllocFailed e)

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
