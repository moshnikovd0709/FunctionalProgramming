module Block1.Task1Spec
  ( spec,
  )
where

import Block1.Task1 (stringSum)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "StringSum" $ do
    it "OK" $ stringSum "11" `shouldBe` Just 11
    it "OK" $ stringSum "1 20 31" `shouldBe` Just 52
    it "OK" $ stringSum "12 15 -13 -12 0" `shouldBe` Just 2
    it "OK" $ stringSum "1 -5 gh1" `shouldBe` Nothing
    it "OK" $ stringSum "aafnfn" `shouldBe` Nothing
    it "OK" $ stringSum "" `shouldBe` Just 0