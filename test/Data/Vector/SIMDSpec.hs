{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Vector.SIMDSpec (spec) where

import Data.Int
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector.SIMD as V
import qualified Data.Vector.Storable as V

instance (V.Storable a, Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary
    shrink = map V.fromList . shrink . V.toList

sumSpec :: forall a. (Ord a, Show a, Eq a, Arbitrary a, V.Storable a, Num a, V.SIMDVecVal a) => String -> a -> Spec
sumSpec name d =
    describe ("sums for " ++ name) $
    do it "smid sum should produce correct values" $
           do V.ssum (V.fromList [1, 10]) `shouldBe` (11 :: a)
              V.ssum (V.fromList [1, 2, 3, 4]) `shouldBe` (10 :: a)
              V.ssum (V.fromList [1, 2, 3, 4, 5]) `shouldBe` (15 :: a)
              V.ssum (V.fromList []) `shouldBe` (0 :: a)
       it "simd sum should match sum implementation" $
           property $ \(vec :: V.Vector a) ->
                          abs (V.sum vec - V.ssum vec) < d

spec :: Spec
spec =
    do sumSpec @Int32 "int32" 1
       sumSpec @Int64 "int64" 1
       sumSpec @Double "double" 0.001
