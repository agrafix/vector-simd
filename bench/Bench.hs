{-# LANGUAGE TypeApplications #-}
module Main where

import Criterion
import Criterion.Main
import Data.Int
import qualified Data.Vector.SIMD as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG

main :: IO ()
main = do
  let n = 1000
  defaultMain
    [ bgroup
        "Int32"
        [ mkGroup "Sum Storable" (VG.enumFromN @VS.Vector @Int32 0 n)
        , mkGroup "Sum Primitive" (VG.enumFromN @VP.Vector @Int32 0 n)
        ]
    , bgroup
        "Int64"
        [ mkGroup "Sum Storable" (VG.enumFromN @VS.Vector @Int64 0 n)
        , mkGroup "Sum Primitive" (VG.enumFromN @VP.Vector @Int64 0 n)
        ]
    , bgroup
        "Double"
        [ mkGroup "Sum Storable" (VG.enumFromN @VS.Vector @Double 0 n)
        , mkGroup "Sum Primitive" (VG.enumFromN @VP.Vector @Double 0 n)
        ]
    ]

mkGroup :: (V.SIMDVecVal v a, VG.Vector v a, Num a) => String -> v a -> Benchmark
mkGroup name v =
    bgroup name
    [ bench "vector implementation" $ whnf VG.sum v
    , bench "simd implementation" $ whnf V.ssum v
    ]
