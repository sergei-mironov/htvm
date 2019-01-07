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
{-# LANGUAGE OverloadedStrings #-}

module HTVM.Runtime.TVMData where

import qualified Data.Array as Array

import Control.Exception (throwIO)
import Control.Monad (when, forM, forM_)
import Control.Arrow ((***))
import Data.Array (Array(..))
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Text (Text)
import Data.List (nub)
import Data.Maybe (maybe, fromJust)
import Foreign (newForeignPtr, Ptr, Storable(..), alloca,
                allocaArray, peek, poke, peekArray, pokeArray,
                castPtr, advancePtr, malloc, mallocArray,
                withForeignPtr, Storable(..), withArray)
import HTVM.Prelude
import HTVM.Runtime.FFI
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Vector.Unboxed as VU

-- | Utilitary class to convert tuples to lists
class TupleList i a | i -> a where
  tuplist :: i -> [a]
  tupdims :: Integer

instance TupleList (a,a) a where tuplist (a,b) = [a,b] ; tupdims = 2 ;
instance TupleList (a,a,a) a where tuplist (a,b,c) = [a,b,c] ; tupdims = 3 ;
instance TupleList (a,a,a,a) a where tuplist (a,b,c,d) = [a,b,c,d] ; tupdims = 4 ;

-- | Class encodes data container interface, accepted by TVM model. In order to
-- be convertible to TVM tensor, Haskell data container @d@ should be able to
-- report its shape, type and raw data
--
-- FIXME: rename tvmXXX -> tensorXXX
-- FIXME: rename TensorDataLike
class TVMData d where
  tvmStaticDataType :: Maybe TensorDataType
  tvmDataType :: d -> TensorDataType
  tvmDataType = const $ fromJust (tvmStaticDataType @d)
  -- ^ Transalte type information into TVM terms
  tvmStaticNDims :: Maybe Integer
  tvmNDims :: d -> Integer
  tvmNDims = const $ fromJust (tvmStaticNDims @d)
  -- ^ Number of dimentions in the Container
  tvmStaticIShape :: Maybe [Integer]
  tvmIShape :: d -> [Integer]
  tvmIShape = const $ fromJust (tvmStaticIShape @d)
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
      sz = tensorDataTypeArraySize (tvmIShape d) (tvmDataType d)
    in do
    allocaArray (fromInteger sz) $ \parr -> do
      tvmPoke d parr
      dat <- peekArray (fromInteger sz) parr
      return (TensorData (tvmIShape d) (tvmDataType d) dat)


instance (Storable e, Array.Ix i, TupleList i Integer, TensorDataTypeRepr e) => TVMData (Array i e) where
  tvmStaticDataType = Just (tensorDataType @e)
  tvmStaticNDims = Just (tupdims @i)
  tvmStaticIShape = Nothing
  tvmIShape = map (uncurry (-)) . uncurry zip . (tuplist *** tuplist) . Array.bounds
  tvmPoke d ptr = pokeArray (castPtr ptr) (Array.elems d)
  tvmPeek shape ptr = error "FIXME: tvmPeek is not implemented for Data.Array"

tvmPoke0 d ptr = poke (castPtr ptr) d
tvmPeek0 :: forall e . (Storable e, TensorDataTypeRepr e) => TensorDataType -> [Integer] -> Ptr Word8 -> IO e
tvmPeek0 typ shape ptr =
  case typ == tensorDataType @e of
    True ->
      case shape of
        [] -> peek (castPtr ptr)
        sh -> throwIO $ DimMismatch (ilength sh) 0
    False -> throwIO $ TypeMismatch "tvmPeek0" typ (tensorDataType @e)

instance TVMData Word8 where tvmStaticDataType = Just $ tensorDataType @Word8; tvmStaticNDims = Just 0; tvmStaticIShape = Just []; tvmPoke = tvmPoke0; tvmPeek = tvmPeek0
instance TVMData Int32 where tvmStaticDataType = Just $ tensorDataType @Int32; tvmStaticNDims = Just 0; tvmStaticIShape = Just []; tvmPoke = tvmPoke0; tvmPeek = tvmPeek0
instance TVMData Word32 where tvmStaticDataType = Just $ tensorDataType @Word32; tvmStaticNDims = Just 0; tvmStaticIShape = Just []; tvmPoke = tvmPoke0; tvmPeek = tvmPeek0
instance TVMData Float where tvmStaticDataType = Just $ tensorDataType @Float; tvmStaticNDims = Just 0; tvmStaticIShape = Just []; tvmPoke = tvmPoke0; tvmPeek = tvmPeek0
instance TVMData Int64 where tvmStaticDataType = Just $ tensorDataType @Int64; tvmStaticNDims = Just 0; tvmStaticIShape = Just []; tvmPoke = tvmPoke0; tvmPeek = tvmPeek0
instance TVMData Word64 where tvmStaticDataType = Just $ tensorDataType @Word64; tvmStaticNDims = Just 0; tvmStaticIShape = Just []; tvmPoke = tvmPoke0; tvmPeek = tvmPeek0
instance TVMData Double where tvmStaticDataType = Just $ tensorDataType @Double; tvmStaticNDims = Just 0; tvmStaticIShape = Just []; tvmPoke = tvmPoke0; tvmPeek = tvmPeek0

tvmIShape1 d = [ilength d]
tvmPoke1 d ptr = pokeArray (castPtr ptr) d
tvmPeek1 :: forall e . (Storable e, TensorDataTypeRepr e) => TensorDataType -> [Integer] -> Ptr Word8 -> IO [e]
tvmPeek1 typ shape ptr =
  case typ == tensorDataType @e of
    True ->
      case shape of
        [x] -> peekArray (fromInteger x) (castPtr ptr)
        sh -> throwIO $ DimMismatch (ilength sh) 1
    False -> throwIO $ TypeMismatch "tvmPeek1" typ (tensorDataType @e)

instance TVMData x => TVMData [x] where
  tvmStaticDataType = tvmStaticDataType @x
  tvmDataType [] = maybe (error "tvmDataType: empty list") id (tvmStaticDataType @x)
  tvmDataType (x:_) = tvmDataType x
  tvmStaticNDims = (1+) <$> tvmStaticNDims @x
  tvmNDims d = 1 + tvmNDims d
  tvmStaticIShape = Nothing
  tvmIShape [] = maybe (error "tvmIShape: unknown shape") (0:) (tvmStaticIShape @x)
  tvmIShape (x:xs) = (1 + ilength xs):(tvmIShape x)
  tvmPeek t [] ptr = error "tvmPeek: attempt to peek from scalar-ptr to list"
  tvmPeek t (s:sh) ptr =
    forM [0..s-1] $ \i -> do
      tvmPeek t sh (advancePtr ptr (fromInteger $ i*(tensorDataTypeArraySize sh t)))
  tvmPoke [] ptr = return () -- error "tvmPoke: attempt to poke from empty list"
  tvmPoke l ptr =
    let
      s:sh = tvmIShape l
      t = tvmDataType l
    in
    forM_ [0..s-1] $ \i ->
      tvmPoke (l!!(fromInteger i)) (advancePtr ptr (fromInteger $ i*(tensorDataTypeArraySize sh t)))

instance (VU.Unbox e, TVMData e, Storable e, TensorDataTypeRepr e) => TVMData (VU.Vector e) where
  tvmStaticDataType = Just (tensorDataType @e)
  tvmStaticNDims = Just 1
  tvmStaticIShape = Nothing
  tvmIShape d = [toInteger $ VU.length d]
  tvmPoke d ptr = tvmPoke1 (VU.toList d) ptr
  tvmPeek typ shape ptr = VU.fromList <$> tvmPeek1 typ shape ptr

tvmDataShape :: (TVMData d) => d -> [Integer]
tvmDataShape = tvmIShape

tvmDataNDim :: (TVMData d) => d -> Integer
tvmDataNDim = ilength . tvmDataShape

-- | Flattens any `TensorLike d` into list of doubles.
flattenReal :: (TVMData d) => d -> [Double]
flattenReal d =
  let
    td = toTD d
    sh1 = [foldr1 (*) (if null $ td_shape td then [1] else td_shape td)]
  in
  case td_type td of
    TD_UInt8L1 -> map (fromRational . toRational) $ fromTD @[Word8] td{td_shape=sh1}
    TD_SInt32L1 -> map (fromRational . toRational) $ fromTD @[Int32] td{td_shape=sh1}
    TD_UInt32L1 -> map (fromRational . toRational) $ fromTD @[Word32] td{td_shape=sh1}
    TD_Float32L1 -> map (fromRational . toRational) $ fromTD @[Float] td{td_shape=sh1}
    TD_SInt64L1 -> map (fromRational . toRational) $ fromTD @[Int64] td{td_shape=sh1}
    TD_UInt64L1 -> map (fromRational . toRational) $ fromTD @[Word64] td{td_shape=sh1}
    TD_Float64L1 -> fromTD @[Double] td{td_shape=sh1}

instance TVMData TensorData where
  tvmStaticDataType = Nothing
  tvmDataType = td_type
  tvmStaticNDims = Nothing
  tvmNDims = ilength . tvmIShape
  tvmStaticIShape = Nothing
  tvmIShape = td_shape
  tvmPeek tp shape ptr =
    TensorData
      <$> pure shape
      <*> pure tp
      <*> tvmPeek1 (tensorDataType @Word8) [tensorDataTypeSize tp * (foldr1 (*) shape)] ptr
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

-- | Transfer data from TVM tensor object to Haskell data container
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
    throwIO (TypeMismatch "pokeTensor" typ (tvmDataType d))
  case tvmTensorDevice ft of
    KDLCPU -> do
      tvmPoke d (unsafeTvmTensorData ft)
    x -> do
      allocaArray (fromInteger $ tvmTensorSize ft) $ \parr -> do
      withForeignPtr ft $ \pt -> do
        tvmPoke d parr
        _ <- tvmArrayCopyFromBytes pt parr (toCSize $ tvmTensorSize ft)
        return ()

