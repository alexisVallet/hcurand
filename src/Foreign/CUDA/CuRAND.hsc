{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.CUDA.CuRAND where

import Foreign
import Foreign.C
import Foreign.CUDA.Types

#include <curand.h>

-- Enums for status, rng, ordering, direction vector set, method.
-- Datatypes for generators, distributions.
newtype Status = Status {
  unStatus :: CInt
  } deriving (Show, Eq, Storable)

#{enum Status, Status
 , status_success = CURAND_STATUS_SUCCESS
 , status_version_mismatch = CURAND_STATUS_VERSION_MISMATCH
 , status_not_initialized = CURAND_STATUS_NOT_INITIALIZED
 , status_allocation_failed = CURAND_STATUS_ALLOCATION_FAILED
 , status_type_error = CURAND_STATUS_TYPE_ERROR
 , status_out_of_range = CURAND_STATUS_OUT_OF_RANGE
 , status_length_not_multiple = CURAND_STATUS_LENGTH_NOT_MULTIPLE
 , status_double_precision_required = CURAND_STATUS_DOUBLE_PRECISION_REQUIRED
 , status_launch_failure = CURAND_STATUS_LAUNCH_FAILURE
 , status_preexisting_failure = CURAND_STATUS_PREEXISTING_FAILURE
 , status_initialization_failed = CURAND_STATUS_INITIALIZATION_FAILED
 , status_arch_mismatch = CURAND_STATUS_ARCH_MISMATCH
 , status_internal_error = CURAND_STATUS_INTERNAL_ERROR
 }

newtype RngType = RngType {
  unRngType :: CInt
  } deriving (Show, Eq, Storable)

#{enum RngType, RngType
 , rng_test = CURAND_RNG_TEST
 , rng_pseudo_default = CURAND_RNG_PSEUDO_DEFAULT
 , rng_pseudo_xorwow = CURAND_RNG_PSEUDO_XORWOW
 , rng_pseudo_mrg32k3a = CURAND_RNG_PSEUDO_MRG32K3A
 , rng_pseudo_mtgp32 = CURAND_RNG_PSEUDO_MTGP32
 , rng_pseudo_mt19937 = CURAND_RNG_PSEUDO_MT19937
 , rng_pseudo_philox4_32_10 = CURAND_RNG_PSEUDO_PHILOX4_32_10
 , rng_quasi_default = CURAND_RNG_QUASI_DEFAULT
 , rng_quasi_sobol32 = CURAND_RNG_QUASI_SOBOL32
 , rng_quasi_scrambled_sobol32 = CURAND_RNG_QUASI_SCRAMBLED_SOBOL32
 , rng_quasi_sobol64 = CURAND_RNG_QUASI_SOBOL64
 , rng_quasi_scrambled_sobol64 = CURAND_RNG_QUASI_SCRAMBLED_SOBOL64
 }

newtype CuRANDOrdering = CuRANDOrdering {
  unCuRANDOrdering :: CInt
  } deriving (Show, Eq, Storable)

#{enum CuRANDOrdering, CuRANDOrdering
 , ordering_pseudo_best = CURAND_ORDERING_PSEUDO_BEST
 , ordering_pseudo_default = CURAND_ORDERING_PSEUDO_DEFAULT
 , ordering_pseudo_seeded = CURAND_ORDERING_PSEUDO_SEEDED
 , ordering_quasi_default = CURAND_ORDERING_QUASI_DEFAULT
 }

newtype DirectionVectorSet = DirectionVectorSet {
  unDirectionVectorSet :: CInt
  } deriving (Show, Eq, Storable)

newtype DirectionVectors32 = DirectionVectors32 {
  unDirectionVectors32 :: Ptr CUInt
  } deriving Storable

newtype DirectionVectors64 = DirectionVectors64 {
  unDirectionVectors64 :: Ptr CULLong
  }

#{enum DirectionVectorSet, DirectionVectorSet
 , direction_vectors_32_joekuo6 = CURAND_DIRECTION_VECTORS_32_JOEKUO6
 , scrambled_direction_vectors_32_joekuo6 = CURAND_SCRAMBLED_DIRECTION_VECTORS_32_JOEKUO6
 , direction_vectors_64_joekuo6 = CURAND_DIRECTION_VECTORS_64_JOEKUO6
 , scrambled_direction_vectors_64_joekuo6 = CURAND_SCRAMBLED_DIRECTION_VECTORS_64_JOEKUO6
 }

newtype Generator = Generator {
  unGenerator :: Ptr ()
  } deriving Storable

newtype Distribution = Distribution {
  unDistribution :: Ptr CDouble
  } deriving (Show, Eq, Storable)

newtype DistributionShift = DistributionShift {
  unDistributionShift :: Ptr ()
  } deriving Storable

newtype DistributionM2Shift = DistributionM2Shift {
  unDistributionM2Shift :: Ptr ()
  } deriving Storable

newtype HistogramM2 = HistogramM2 {
  unHistogramM2 :: Ptr ()
  } deriving Storable

newtype HistogramM2K = HistogramM2K {
  unHistogramM2K :: Ptr CInt
  } deriving (Show, Eq, Storable)

newtype HistogramM2V = HistogramM2V {
  unHistogramM2V :: Ptr Distribution
  } deriving (Show, Eq, Storable)

newtype DiscreteDistribution = DiscreteDistribution {
  unDiscreteDistribution :: Ptr ()
  }

newtype Method = Method {
  unMethod :: CInt
  } deriving (Show, Eq, Storable)

#{enum Method, Method
 , choose_best = CURAND_CHOOSE_BEST
 , itr = CURAND_ITR
 , knuth = CURAND_KNUTH
 , hitr = CURAND_HITR
 , m1 = CURAND_M1
 , m2 = CURAND_M2
 , binary_search = CURAND_BINARY_SEARCH
 , discrete_gauss = CURAND_DISCRETE_GAUSS
 , rejection = CURAND_REJECTION
 , device_api = CURAND_DEVICE_API
 , fast_rejection = CURAND_FAST_REJECTION
 , _3rd  = CURAND_3RD
 , definition = CURAND_DEFINITION
 , poisson = CURAND_POISSON
 }

-- Random number generator manipulation functions.
foreign import ccall unsafe "curandCreateGenerator"
  createGenerator :: Ptr Generator -> RngType -> IO Status

foreign import ccall unsafe "curandCreateGeneratorHost"
  createGeneratorHost :: Ptr Generator -> RngType -> IO Status

foreign import ccall unsafe "curandDestroyGenerator"
  destroyGenerator :: Generator -> IO Status

-- Library version.
foreign import ccall unsafe "curandGetVersion"
  getVersion :: Ptr CInt -> IO Status

-- Set the CUDA stream.
foreign import ccall unsafe "curandSetStream"
  setStream :: Generator -> Stream -> IO Status

-- Seeding.
foreign import ccall unsafe "curandSetPseudoRandomGeneratorSeed"
  setPseudoRandomGeneratorSeed :: Generator -> CULLong -> IO Status

foreign import ccall unsafe "curandSetGeneratorOffset"
  setGeneratorOffset :: Generator -> CULLong -> IO Status

-- CuRANDOrdering.
foreign import ccall unsafe "curandSetGeneratorCuRANDOrdering"
  setGeneratorCuRANDOrdering :: Generator -> CuRANDOrdering -> IO Status

-- Dimensions.
foreign import ccall unsafe "curandSetQuasiRandomGeneratorDimensions"
  setQuasiRandomGeneratorDimensions :: Generator -> CUInt -> IO Status

-- Random number generation.
foreign import ccall unsafe "curandGenerate"
  generate :: Generator -> DevicePtr CUInt -> CSize -> IO Status

foreign import ccall unsafe "curandGenerateLongLong"
  generateLongLong :: Generator -> DevicePtr CULLong -> CSize -> IO Status

-- Uniform distribution.
foreign import ccall unsafe "curandGenerateUniform"
  generateUniform :: Generator -> DevicePtr CFloat -> CSize -> IO Status

foreign import ccall unsafe "curandGenerateUniformDouble"
  generateUniformDouble :: Generator -> DevicePtr CDouble -> CSize -> IO Status

-- Normal distribution.
foreign import ccall unsafe "curandGenerateNormal"
  generateNormal :: Generator -> DevicePtr CFloat -> CSize -> CFloat -> CFloat
                 -> IO Status

foreign import ccall unsafe "curandGenerateNormalDouble"
  generateNormalDouble :: Generator -> DevicePtr CDouble -> CSize -> CDouble -> CDouble
                       -> IO Status

-- Log-normal distribution.
foreign import ccall unsafe "curandGenerateLogNormal"
  generateLogNormal :: Generator -> DevicePtr CFloat -> CSize -> CFloat -> CFloat
                    -> IO Status

foreign import ccall unsafe "curandGenerateLogNormalDouble"
  generateLogNormalDouble :: Generator -> DevicePtr CDouble -> CSize -> CDouble
                          -> CDouble -> IO Status

-- Poisson distribution.
foreign import ccall unsafe "curandCreatePoissonDistribution"
  createPoissonDistribution :: CDouble -> Ptr DiscreteDistribution -> IO Status

foreign import ccall unsafe "curandDestroyDistribution"
  destroyDistribution :: DiscreteDistribution -> IO Status

foreign import ccall unsafe "curandGeneratePoisson"
  generatePoisson :: Generator -> DevicePtr CUInt -> CSize -> CDouble -> IO Status

foreign import ccall unsafe "curandGeneratePoissonMethod"
  generatePoissonMethod :: Generator -> DevicePtr CUInt -> CSize -> CDouble
                        -> IO Status

-- Setting up starting states.
foreign import ccall unsafe "curandGenerateSeeds"
  generateSeeds :: Generator -> IO Status

foreign import ccall unsafe "curandGetDirectionVectors32"
  getDirectionVectors32 :: Ptr (Ptr DirectionVectors32) -> DirectionVectorSet
                        -> IO Status

foreign import ccall unsafe "curandGetScrambleConstants32"
  getScrambleConstants32 :: Ptr (Ptr CUInt) -> IO Status

foreign import ccall unsafe "curandGetDirectionVectors64"
  getDirectionVectors64 :: Ptr (Ptr DirectionVectors64) -> DirectionVectors64
                        -> IO Status

foreign import ccall unsafe "curandGetScrambleConstants64"
  getScrambleConstants64 :: Ptr (Ptr CULLong) -> IO Status

