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

module HTVM.Runtime.TVMData where

import qualified Data.Array as Array

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Arrow ((***))
import Data.Array (Array(..))
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Text (Text)
import Data.List (nub)
import Foreign (newForeignPtr, Ptr, Storable(..), alloca,
                allocaArray, peek, poke, peekArray, pokeArray,
                castPtr, advancePtr, malloc, mallocArray,
                withForeignPtr, Storable(..), withArray)

import HTVM.Prelude
import HTVM.Runtime.FFI
import System.IO.Unsafe (unsafePerformIO)


-- | Utilitary class to convert tuples to lists
class TupleList i a | i -> a where
  tuplist :: i -> [a]

instance TupleList (a,a) a where tuplist (a,b) = [a,b]
instance TupleList (a,a,a) a where tuplist (a,b,c) = [a,b,c]
instance TupleList (a,a,a,a) a where tuplist (a,b,c,d) = [a,b,c,d]

-- | Class encodes data container interface, accepted by TVM model. In order to
-- be convertible to TVM tensor, Haskell data container @d@ should be able to
-- report its shape, type and raw data
--
-- FIXME: rename tvmXXX -> tensorXXX
-- FIXME: rename TensorDataLike
class TVMData d where
  tvmDataType :: d -> TensorDataType
  -- ^ Transalte type information into TVM terms
  tvmIShape :: d -> [Integer]
  -- ^ Get the shape of data, in form of list of inegers
  tvmPeek :: TensorDataType -> [Integer] -> Ptr Word8 -> IO d
  -- ^ Take the type, the shape and the pointer to dense memory area of size
  -- `dataArraySize`, produce the data container.
  tvmPoke :: d -> Ptr Word8 -> IO ()
  -- ^ Write the data @d@ to dense memory area of size `dataArraySize`.
  -- TODO: figure out the alignment restirctions.
  fromTD :: TensorData -> d
  fromTD TensorData{..} = unsafePerformIO $ do
    withArray td_data $ \parr -> do
      tvmPeek td_type td_shape parr

  toTD :: d -> TensorData
  toTD d = unsafePerformIO $
    let
      sz = tensorDataTypeSize (tvmIShape d) (tvmDataType d)
    in do
    allocaArray (fromInteger sz) $ \parr -> do
      tvmPoke d parr
      dat <- peekArray (fromInteger sz) parr
      return (TensorData (tvmIShape d) (tvmDataType d) dat)


instance (Storable e, Array.Ix i, TupleList i Integer, TensorDataTypeRepr e) => TVMData (Array i e) where
  tvmDataType = const (tensorDataType @e)
  -- flattern = error "FIXME: flattern is not implemented for Data.Array"
  tvmIShape = map (uncurry (-)) . uncurry zip . (tuplist *** tuplist) . Array.bounds
  tvmPoke d ptr = pokeArray (castPtr ptr) (Array.elems d)
  tvmPeek shape ptr = error "FIXME: tvmPeek is not implemented for Data.Array"

tvmIShape1 d = [ilength d]
tvmPoke1 d ptr = pokeArray (castPtr ptr) d
tvmPeek1 :: forall e . (Storable e, TensorDataTypeRepr e) => TensorDataType -> [Integer] -> Ptr Word8 -> IO [e]
tvmPeek1 typ shape ptr =
  case typ == tensorDataType @e of
    True ->
      case shape of
        [x] -> peekArray (fromInteger x) (castPtr ptr)
        sh -> throwIO $ DimMismatch (ilength sh) 1
    False -> throwIO $ TypeMismatch typ (tensorDataType @e)

instance TVMData [Word8] where tvmDataType = const (tensorDataType @Word8); tvmIShape = tvmIShape1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Int32] where tvmDataType = const (tensorDataType @Int32); tvmIShape = tvmIShape1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Word32] where tvmDataType = const (tensorDataType @Word32); tvmIShape = tvmIShape1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Float] where tvmDataType = const (tensorDataType @Float); tvmIShape = tvmIShape1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Int64] where tvmDataType = const (tensorDataType @Int64); tvmIShape = tvmIShape1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Word64] where tvmDataType = const (tensorDataType @Word64); tvmIShape = tvmIShape1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1
instance TVMData [Double] where tvmDataType = const (tensorDataType @Double); tvmIShape = tvmIShape1; tvmPoke = tvmPoke1; tvmPeek = tvmPeek1

tvmIShape2 [] = [0,0]
tvmIShape2 d = [ilength d, ilength (head d)]
tvmPoke2 d ptr
  | length d == 0 = pokeArray (castPtr ptr) (concat d)
  | length (nub (map length d)) == 1 = pokeArray (castPtr ptr) (concat d)
  | otherwise = error "All elements should have the same length"
tvmPeek2 :: forall e . (Storable e, TensorDataTypeRepr e) => TensorDataType -> [Integer] -> Ptr Word8 -> IO [[e]]
tvmPeek2 typ shape ptr =
  let
    group :: Integer -> [a] -> [[a]]
    group _ [] = []
    group n l
      | n > 0 = (take (fromInteger n) l) : (group n (drop (fromInteger n) l))
      | otherwise = error "tvmPeek2: Negative n"
  in
  case typ == tensorDataType @e of
    False -> throwIO $ TypeMismatch typ (tensorDataType @e)
    True ->
      case shape of
        [0,0] -> pure []
        [x,y] -> group y <$> tvmPeek1 typ [x*y] ptr where
        _ -> throwIO $ DimMismatch (ilength shape) 2
instance (Storable e, TensorDataTypeRepr e) => TVMData [[e]] where
  tvmDataType = const (tensorDataType @e)
  tvmIShape = tvmIShape2
  tvmPoke = tvmPoke2
  tvmPeek = tvmPeek2

tvmDataShape :: (TVMData d) => d -> [Integer]
tvmDataShape = tvmIShape

tvmDataNDim :: (TVMData d) => d -> Integer
tvmDataNDim = ilength . tvmDataShape

flatternFloat :: TensorData -> [Double]
flatternFloat td =
  let
    sh1 = [foldr1 (*) (td_shape td)]
  in
  case td_type td of
    TD_UInt8L1 -> map (fromRational . toRational) $ (fromTD td{td_shape=sh1} :: [Word8])
    TD_SInt32L1 -> map (fromRational . toRational) $ (fromTD td{td_shape=sh1} :: [Int32])
    TD_UInt32L1 -> map (fromRational . toRational) $ (fromTD td{td_shape=sh1} :: [Word32])
    TD_Float32L1 -> map (fromRational . toRational) $ (fromTD td{td_shape=sh1} :: [Float])
    TD_SInt64L1 -> map (fromRational . toRational) $ (fromTD td{td_shape=sh1} :: [Int64])
    TD_UInt64L1 -> map (fromRational . toRational) $ (fromTD td{td_shape=sh1} :: [Word64])
    TD_Float64L1 -> fromTD td{td_shape=sh1} :: [Double]

instance TVMData TensorData where
  tvmDataType = td_type
  tvmIShape = td_shape
  tvmPeek tp shape ptr = TensorData <$> pure shape <*> pure tp <*> tvmPeek1 tp [foldr1 (*) shape] ptr
  tvmPoke (TensorData sh typ d) ptr = tvmPoke1 d ptr
  toTD = id
  fromTD = id

-- | Create new empty TVMTensor object. This object will be managed by Haskell
-- runtime which would call free when it decides to do so.
--
-- FIXME: non-CPU devices will not work, see FIXME in `pokeTensor`
newEmptyTensor
  :: TVMDataType                 -- ^ TensorType
  -> [Integer]                   -- ^ Shape
  -> TVMDeviceType               -- ^ Device type (CPU|GPU|etc)
  -> TVMDeviceId                 -- ^ Device ID
  -> IO TVMTensor
newEmptyTensor typ shape dt did =
  let
    ndim = length shape
    (TVMDataType code bits lanes) = typ
  in do
  alloca $ \pt -> do
  allocaArray ndim $ \pshape -> do
    pokeArray pshape (map (fromInteger . toInteger) shape)
    r <-
      tvmArrayAlloc
        pshape
        (fromInteger $ toInteger $ ndim)
        (toCInt $ fromEnum code)
        (toCInt $ bits)
        (toCInt $ lanes)
        (toCInt $ fromEnum dt)
        (toCInt $ did) pt
    case r of
      0 -> peek pt >>= newForeignPtr tvmArrayFree_
      err -> throwIO (TVMAllocFailed (fromCInt err))

-- | Allocate new Tensor object
newTensor :: forall d . (TVMData d)
  => d                           -- ^ TvmData tensor-like object
  -> TVMDeviceType               -- ^ Device type
  -> TVMDeviceId                 -- ^ Device ID
  -> IO TVMTensor
newTensor d dt did = do
  ft <- newEmptyTensor (toTvmDataType $ tvmDataType d) (map fromInteger $ tvmDataShape d) dt did
  withForeignPtr ft $ \pt -> do
    tvmPoke d (unsafeTvmTensorData ft)
  return ft

-- | Transfer data from TVMTensor to TVMData instance
peekTensor :: forall d . (TVMData d)
  => TVMTensor -> IO d
peekTensor ft = do
  typ <- tvmTensorDataType ft
  case tvmTensorDevice ft of
    KDLCPU -> do
      tvmPeek typ (tvmTensorShape ft) (unsafeTvmTensorData ft)
    x -> do
      allocaArray (fromInteger $ tvmTensorSize ft) $ \parr -> do
      withForeignPtr ft $ \pt -> do
        _ <- tvmArrayCopyToBytes pt parr (toCSize $ tvmTensorSize ft)
        d <- tvmPeek typ (tvmTensorShape ft) parr
        return d

-- | Transfer data from TVMData instance to TVMTensor
pokeTensor :: forall d . (TVMData d)
  => TVMTensor -> d -> IO ()
pokeTensor ft d = do
  typ <- tvmTensorDataType ft
  when (tvmTensorShape ft /= tvmDataShape d) $
    throwIO (ShapeMismatch (tvmTensorShape ft) (tvmDataShape d))
  when (typ /= tvmDataType d) $
    throwIO (TypeMismatch typ (tvmDataType d))
  case tvmTensorDevice ft of
    KDLCPU -> do
      tvmPoke d (unsafeTvmTensorData ft)
    x -> do
      allocaArray (fromInteger $ tvmTensorSize ft) $ \parr -> do
      withForeignPtr ft $ \pt -> do
        tvmPoke d parr
        _ <- tvmArrayCopyFromBytes pt parr (toCSize $ tvmTensorSize ft)
        return ()

