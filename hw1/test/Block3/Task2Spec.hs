module Block3.Task2Spec (spec) where

import Block3.Task2 (NonEmpty (..), ThisOrThat (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "NonEmpty" $ do
    it "OK" $ 4 :| [3] <> 2 :| [] `shouldBe` 4 :| [3, 2]
    it "OK" $ 0 :| [] <> 1 :| [2] <> 3 :| [4, 5, 6] `shouldBe` 0 :| [1, 2, 3, 4, 5, 6]

  describe "ThisOrThat" $ do
    --it "OK" $ (This "Hi") <> (That 1) `shouldBe` (Both "Hi" 1)
    --it "OK" $ (That 3) <> (That 1) `shouldBe` (That 3)
    --it "OK" $ (This 3) <> (This 1) `shouldBe` (This 3)
    --it "OK" $ (Both "Hi" 1) <> (That 2) `shouldBe` (Both "Hi" 1)
    --it "OK" $ (Both "Hi" 2) <> (This "u") `shouldBe` (Both "Hi" 2)
    --it "OK" $ (This "Hi") <> (Both "l" 2) `shouldBe` (Both "Hi" 1)
    --it "OK" $ (That "Hi") <> (Both "Hi" "u") `shouldBe` (Both "Hi" "Hi")
    --it "OK" $ (Both 1 3) <> (Both 3 1) `shouldBe` (Both 1 3)

