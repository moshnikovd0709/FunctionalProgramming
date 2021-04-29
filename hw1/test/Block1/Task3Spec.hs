module Block1.Task3Spec (spec) where

import Block1.Task3 (SearchTree (..), isEmpty, size, contains, push, treeFromList, pop)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "treeFromList" $ do
    it "OK" $ treeFromList [1, 2, 3, 4, 5] `shouldBe` Node [1] Leaf (Node [2] Leaf (Node [3] Leaf (Node [4] Leaf (Node [5] Leaf Leaf))))
    it "OK" $ treeFromList [3, 2, 1] `shouldBe`  Node [3] (Node [2] (Node [1] Leaf Leaf) Leaf) Leaf

  describe "empty" $ do
    it "OK" $ isEmpty Leaf `shouldBe` True
    it "OK" $ isEmpty (treeFromList [5]) `shouldBe` False

  describe "size" $ do
    it "OK" $ size Leaf `shouldBe` 0
    it "OK" $ size (treeFromList [1, 2, 3, 4, 5]) `shouldBe` 5

  describe "contains" $ do
    it "OK" $ do contains 5 Leaf `shouldBe` False
    it "OK" $ do contains 0 (treeFromList [1, 2, 3, 4, 5]) `shouldBe` False
    it "OK" $ do contains 1 (treeFromList [1, 2, 3]) `shouldBe` True
    it "OK" $ do contains 8 (treeFromList [1, 100, 10, 2, 8, 7]) `shouldBe` True

  describe "push" $ do
    it "OK" $ do push Leaf 0 `shouldBe` Node [0] Leaf Leaf
    it "OK" $ do push (treeFromList [0, 100, 10, 2, 8, 7]) 1 `shouldBe` (treeFromList [0, 100, 10, 2, 8, 7, 1])
    it "OK" $ do push (Node [1] Leaf Leaf) 1 `shouldBe` Node [1, 1] Leaf Leaf

  describe "pop" $ do
    it "OK" $ pop 1 Leaf `shouldBe` Leaf
    it "OK" $ pop 1 (treeFromList [1, 1, 2, 3]) `shouldBe` (treeFromList [1, 2, 3])
    it "OK" $ pop 3 (treeFromList [1, 2, 3, 4, 5]) `shouldBe` Node [1] Leaf (Node [2] Leaf (Node [4] Leaf (Node [5] Leaf Leaf)))

  describe "foldr" $ do
      it "OK" $ foldr (+) 3 (treeFromList [1, 2, 3]) `shouldBe` 9
      it "OK" $ foldr (+) 1 (treeFromList [5, 8, 30]) `shouldBe` 44

  describe "foldMap" $ do
      it "OK" $ foldMap (: []) (treeFromList [1, 2, 3]) `shouldBe` [1, 2, 3]
