module Main where

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.Int
import qualified Data.Vector.SIMD as V
import qualified Data.Vector.Storable as V

main :: IO ()
main =
    defaultMain
    [ bgroup "main"
        [ mkGroup "sum" V.sum V.ssum
        ]
    ]

vLarge :: V.Vector Int32
vLarge = V.replicate 1000 1

mkGroup :: NFData r => String -> (V.Vector Int32 -> r) -> (V.Vector Int32 -> r) -> Benchmark
mkGroup name vec smid =
    bgroup name
    [ bench "vector implementation" $ nf vec vLarge
    , bench "simd implementation" $ nf smid vLarge
    ]
