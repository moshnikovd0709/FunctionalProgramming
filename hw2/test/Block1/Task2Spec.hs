module Block1.Task2Spec
  ( spec,
  )
where

import Block1.Task2 (Tree (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Functor" $ do
    it "OK" $ fmap (+ 3) (Branch (Leaf 1) (Leaf (-2)) :: Tree Int) `shouldBe` Branch (Leaf 4) (Leaf 1)
    it "OK" $ fmap (* 3) (Branch (Leaf 1) (Leaf (-2)) :: Tree Int) `shouldBe` Branch (Leaf 3) (Leaf (-6))

  describe "Applicative" $ do
    it "OK" $ pure id <*> (Branch (Leaf 1) (Leaf 2) :: Tree Int) `shouldBe` (Branch (Leaf 1) (Leaf 2) :: Tree Int)
    it "OK" $ (pure (+ 4) :: Tree (Int -> Int)) <*> pure 1 `shouldBe` pure 5

  describe "Foldable" $ do
    it "OK" $ foldr (+) 3 (Branch (Branch (Leaf 1) (Leaf 3)) (Branch (Leaf 13) (Leaf (-5))) :: Tree Int) `shouldBe` 15
  
  describe "Traversable" $ do
      it "OK" $ traverse (: []) (Branch (Leaf 1) (Leaf 2) :: Tree Int) `shouldBe` [(Branch (Leaf 1) (Leaf 2) :: Tree Int)]