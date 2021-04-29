module Block2.Task2Spec (spec) where

import Block2.Task2 (joinWith, splitOn)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "splitOn" $ do
    it "OK" $ splitOn '/' "path/to/file" `shouldBe` "path" :| ["to", "file"]
    it "OK" $ splitOn 1 [0, 1, 4, 7, 1, 1, 1, 7] `shouldBe` [0] :| [[4, 7], [], [], [7]]
    it "OK" $ splitOn 0 [2, 5, 6] `shouldBe` [2, 5, 6] :| []

  describe "joinWith" $ do
    it "OK" $ join '/' "path/to/file" `shouldBe` "path/to/file" where
    join a = joinWith a . splitOn a
