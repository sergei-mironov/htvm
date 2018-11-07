-- | Module defines wrappers for DLPack messages which are used by TVM to pass
-- to/from models

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}


module HTVM.Runtime.FFI where

import Data.ByteString (ByteString,pack)
import Data.Word (Word8,Word16)
import Data.Bits (FiniteBits(..),(.&.),shiftR)
import Foreign (Ptr, Storable(..), alloca, peek, plusPtr, poke)
import Foreign.C.Types (CInt, CLong)
import System.IO.Unsafe (unsafePerformIO)


#include <dlpack/dlpack.h>
#include <tvm/runtime/c_runtime_api.h>

data DLTensor

instance Storable DLTensor where
  sizeOf _ = {# sizeof DLTensor #}
  alignment _ = {# alignof DLTensor #}
  peek = undefined
  poke = undefined

foreign import ccall unsafe "c_runtime_api.h TVMArrayAlloc"
    tvm_array_alloc
      :: Ptr Int      -- shape
      -> Int          -- ndim,
      -> Int          -- dtype_code,
      -> Int          -- dtype_bits,
      -> Int          -- dtype_lanes,
      -> Int          -- device_type,
      -> Int          -- device_id,
      -> Ptr DLTensor -- DLTensor* out
      -> IO Int


-- dltensor_alloc :: [Integer] -> Integer -> IO (Ptr DLTensor)
-- dltensor_alloc shape ndim dtype dtbits dtlanes devtype devid = do
--   TVMArrayAlloc

dltensor_compute :: Ptr DLTensor -> [Integer] -> ([Integer] -> Float) -> IO (Ptr DLTensor)
dltensor_compute pdlt shape fun =
  let
  in do
  undefined


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
