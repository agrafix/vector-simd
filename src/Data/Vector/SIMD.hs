{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Vector.SIMD
    ( SIMDVecVal(..)
    , V.Vector
    )
where

import Data.Int
import Foreign
import System.IO.Unsafe
import qualified Data.Vector.Storable as V

class SIMDVecVal a where
    ssum :: V.Vector a -> a

instance SIMDVecVal Int32 where
    ssum v =
        unsafePerformIO $
        V.unsafeWith v $ \ptr -> sum_vec_i32 ptr (V.length v)

instance SIMDVecVal Int64 where
    ssum v =
        unsafePerformIO $
        V.unsafeWith v $ \ptr -> sum_vec_i64 ptr (V.length v)

instance SIMDVecVal Double where
    ssum v =
        unsafePerformIO $
        V.unsafeWith v $ \ptr -> sum_vec_dbl ptr (V.length v)

foreign import ccall unsafe "sum_vec_i32" sum_vec_i32 :: Ptr Int32 -> Int -> IO Int32
foreign import ccall unsafe "sum_vec_i64" sum_vec_i64 :: Ptr Int64 -> Int -> IO Int64
foreign import ccall unsafe "sum_vec_dbl" sum_vec_dbl :: Ptr Double -> Int -> IO Double
