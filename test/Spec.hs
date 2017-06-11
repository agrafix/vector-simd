import Test.Hspec

import qualified Data.Vector.SIMDSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do describe "Data.Vector.SIMD" Data.Vector.SIMDSpec.spec
