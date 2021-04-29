module Block3.Task1Spec (spec) where

import Block3.Task1 (maybeConcat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "maybeConcat" $ do
    it "OK" $ maybeConcat [Just [13, 40, 23], Nothing, Nothing, Just [17, 4], Just [1 , 2], Nothing] `shouldBe` [13, 40, 23, 17, 4, 1, 2]
    it "OK" $ maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5]
    it "OK" $ maybeConcat ([Nothing] :: [Maybe [Integer]]) `shouldBe` []
    it "OK" $ maybeConcat [Just [1, 40, 23]] `shouldBe` [1, 40, 23]