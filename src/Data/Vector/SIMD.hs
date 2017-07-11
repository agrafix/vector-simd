{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE UnboxedTuples            #-}
module Data.Vector.SIMD
    ( SIMDVecVal(..)
    )
where

import           Data.Int
import           Data.Primitive.ByteArray
import qualified Data.Vector.Generic      as VG
import qualified Data.Vector.Primitive    as VP
import qualified Data.Vector.Storable     as VS
import           Foreign
import           GHC.Float
import           GHC.Int
import           GHC.Prim
import           System.IO.Unsafe

class VG.Vector v a => SIMDVecVal v a where
    ssum :: v a -> a

instance SIMDVecVal VS.Vector Int32 where
    ssum v =
        unsafePerformIO $
        VS.unsafeWith v $ \ptr -> sum_vec_i32 ptr (VS.length v)

instance SIMDVecVal VS.Vector Int64 where
    ssum v =
        unsafePerformIO $
        VS.unsafeWith v $ \ptr -> sum_vec_i64 ptr (VS.length v)

instance SIMDVecVal VS.Vector Double where
    ssum v =
        unsafePerformIO $
        VS.unsafeWith v $ \ptr -> sum_vec_dbl ptr (VS.length v)

foreign import ccall unsafe "sum_vec_i32" sum_vec_i32 :: Ptr Int32 -> Int -> IO Int32
foreign import ccall unsafe "sum_vec_i64" sum_vec_i64 :: Ptr Int64 -> Int -> IO Int64
foreign import ccall unsafe "sum_vec_dbl" sum_vec_dbl :: Ptr Double -> Int -> IO Double


instance SIMDVecVal VP.Vector Int32 where
    ssum (VP.Vector _ n ba) = sumByteArrayInt32X4 ba n


instance SIMDVecVal VP.Vector Int64 where
    ssum (VP.Vector _ n ba) = sumByteArrayInt64X2 ba n


instance SIMDVecVal VP.Vector Double where
    ssum (VP.Vector _ n ba) = sumByteArrayDoubleX2 ba n


sumByteArrayInt32X4 :: ByteArray -> Int -> Int32
sumByteArrayInt32X4 (ByteArray ba#) (I# n#) = I32# (addRest# (x0# +# x1# +# x2# +# x3#) q#)
  where
    q# = n# -# (n# `remInt#` 4#)
    (# x0#, x1#, x2#, x3# #) = unpackInt32X4# (goInt32X4# (broadcastInt32X4# 0#) 0#)
    goInt32X4# acc# i# =
      case i# <# q# of
        0# -> acc#
        _  -> goInt32X4# (plusInt32X4# acc# (indexInt32ArrayAsInt32X4# ba# i#)) (i# +# 4#)
    addRest# acc# i# =
      case i# <# n# of
        0# -> acc#
        _  -> addRest# (acc# +# indexInt32Array# ba# i#) (i# +# 1#)



sumByteArrayInt64X2 :: ByteArray -> Int -> Int64
sumByteArrayInt64X2 (ByteArray ba#) (I# n#) = I64# (addRest# (x0# +# x1#) q#)
  where
    q# = n# -# (n# `remInt#` 2#)
    (# x0#, x1# #) = unpackInt64X2# (goInt64X2# (broadcastInt64X2# 0#) 0#)
    goInt64X2# acc# i# =
      case i# <# q# of
        0# -> acc#
        _  -> goInt64X2# (plusInt64X2# acc# (indexInt64ArrayAsInt64X2# ba# i#)) (i# +# 2#)
    addRest# acc# i# =
      case i# <# n# of
        0# -> acc#
        _  -> addRest# (acc# +# indexInt64Array# ba# i#) (i# +# 1#)



sumByteArrayDoubleX2 :: ByteArray -> Int -> Double
sumByteArrayDoubleX2 (ByteArray ba#) (I# n#) = D# (addRest# (x0# +## x1#) q#)
  where
    q# = n# -# (n# `remInt#` 2#)
    (# x0#, x1# #) = unpackDoubleX2# (goDoubleX2# (broadcastDoubleX2# 0.0##) 0#)
    goDoubleX2# acc# i# =
      case i# <# q# of
        0# -> acc#
        _  -> goDoubleX2# (plusDoubleX2# acc# (indexDoubleArrayAsDoubleX2# ba# i#)) (i# +# 2#)
    addRest# acc# i# =
      case i# <# n# of
        0# -> acc#
        _  -> addRest# (acc# +## indexDoubleArray# ba# i#) (i# +# 1#)


-- My CPU only supports 128 bit SIMD
-- sumByteArrayInt64X4 :: ByteArray -> Int -> Int64
-- sumByteArrayInt64X4 (ByteArray ba#) (I# n#) = I64# (addRest# (x0# +# x1# +# x2# +# x3#) q#)
--   where
--     q# = n# -# (n# `remInt#` 4#)
--     (# x0#, x1#, x2#, x3# #) = unpackInt64X4# (goInt64X4# (broadcastInt64X4# 0#) 0#)
--     goInt64X4# acc# i# =
--       case i# <# q# of
--         0# -> acc#
--         _  -> goInt64X4# (plusInt64X4# acc# (indexInt64ArrayAsInt64X4# ba# i#)) (i# +# 4#)
--     addRest# acc# i# =
--       case i# <# n# of
--         0# -> acc#
--         _  -> addRest# (acc# +# indexInt64Array# ba# i#) (i# +# 1#)
