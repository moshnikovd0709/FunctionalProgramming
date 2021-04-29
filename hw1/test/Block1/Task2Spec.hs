module Block1.Task2Spec (spec) where

import Block1.Task2 (Nat (..), convertNatToInt, convertIntToNat)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "convertIntToNat" $ do
    it "OK" $ (convertIntToNat) 0 `shouldBe` Z
    it "OK" $ (convertIntToNat 2) `shouldBe` S (S Z)

  describe "+" $ do
    it "OK" $ (convertIntToNat 0) + (convertIntToNat 5) `shouldBe` (convertIntToNat 5)
    it "OK" $ (convertIntToNat 12) + (convertIntToNat 7) `shouldBe` (convertIntToNat 19)

  describe "-" $ do
    it "Ok" $ (convertIntToNat 0) - (convertIntToNat 3) `shouldBe` Z
    it "OK" $ (convertIntToNat 10) - (convertIntToNat 5) `shouldBe` (convertIntToNat 5)
    it "OK" $ (convertIntToNat 6) - (convertIntToNat 0) `shouldBe` (convertIntToNat 6)

  describe "*" $ do
    it "OK" $ (convertIntToNat 1) * (convertIntToNat 5) `shouldBe` (convertIntToNat 5)
    it "OK" $ (convertIntToNat 2) * (convertIntToNat 2) `shouldBe` (convertIntToNat 4)
    it "OK" $ (convertIntToNat 1) * (convertIntToNat 0) `shouldBe` Z

  describe "convertNatToInt" $ do
      it "OK" $ convertNatToInt (convertIntToNat 0) `shouldBe` 0
      it "OK" $ convertNatToInt (convertIntToNat 1) `shouldBe` 1
      it "OK" $ convertNatToInt (convertIntToNat 77) `shouldBe` 77

  describe "==" $ do
      it "OK" $ (convertIntToNat 6) == (convertIntToNat 6) `shouldBe` True
      it "OK" $ (convertIntToNat 0) == (convertIntToNat 1) `shouldBe` False

  describe ">=" $ do
      it "OK" $ (convertIntToNat 1) <= (convertIntToNat 1) `shouldBe` True
      it "OK" $ (convertIntToNat 30) > (convertIntToNat 30) `shouldBe` False
      it "OK" $ (convertIntToNat 21) >= (convertIntToNat 3) `shouldBe` True

