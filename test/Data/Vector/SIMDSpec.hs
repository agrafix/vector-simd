{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Vector.SIMDSpec (spec) where

import Data.Int
import Test.Hspec
import Test.QuickCheck
import Data.Proxy
import qualified Data.Vector.SIMD as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP

instance (VS.Storable a, Arbitrary a) => Arbitrary (VS.Vector a) where
    arbitrary = VS.fromList <$> arbitrary
    shrink = map VS.fromList . shrink . VS.toList


instance (VP.Prim a, Arbitrary a) => Arbitrary (VP.Vector a) where
    arbitrary = VP.fromList <$> arbitrary
    shrink = map VP.fromList . shrink . VP.toList


sumSpec
  :: forall v a.
     (Ord a, Show a, Eq a, Num a, Arbitrary (v a), Show (v a), V.SIMDVecVal v a)
  => Proxy v -> a -> Spec
sumSpec _ d =
    do it "simd sum should produce correct values" $
           do V.ssum (VG.fromList [1, 10] :: v a) `shouldBe` (11 :: a)
              V.ssum (VG.fromList [1, 2, 3, 4] :: v a) `shouldBe` (10 :: a)
              V.ssum (VG.fromList [1, 2, 3, 4, 5] :: v a) `shouldBe` (15 :: a)
              V.ssum (VG.fromList [] :: v a) `shouldBe` (0 :: a)
       it "simd sum should match sum implementation" $
           property $ sumProperty @v d


sumProperty :: (VG.Vector v a, Arbitrary (v a), V.SIMDVecVal v a, Num a, Ord a) => a -> v a -> Bool
sumProperty d vec = abs (VG.sum vec - V.ssum vec) <= d

spec :: Spec
spec = do
  describe "Int32" $ do
    describe "Storable" $ sumSpec (Proxy :: Proxy VS.Vector) (0 :: Int32)
    describe "Primitive" $ sumSpec (Proxy :: Proxy VP.Vector) (0 :: Int32)
  describe "Int64" $ do
    describe "Storable" $ sumSpec (Proxy :: Proxy VS.Vector) (0 :: Int64)
    describe "Primitive" $ sumSpec (Proxy :: Proxy VP.Vector) (0 :: Int64)
  describe "Double" $ do
    describe "Storable" $ sumSpec (Proxy :: Proxy VS.Vector) (0.00001 :: Double)
    describe "Primitive" $ sumSpec (Proxy :: Proxy VP.Vector) (0.00001 :: Double)
