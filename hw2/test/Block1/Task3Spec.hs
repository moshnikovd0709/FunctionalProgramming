module Block1.Task3Spec
  ( spec,
  )
where

import Block1.Task3 (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "functor" $ do
    it "OK" $ fmap (+ 3) ((-3) :| [] :: NonEmpty Int) `shouldBe` 0 :| []
    it "OK" $ fmap (* 3) (1 :| [2, 0, -3] :: NonEmpty Int) `shouldBe` 3 :| [6, 0, -9]
    it "OK" $ fmap ((+ 1) . (* 2)) (1 :| [0, 3] :: NonEmpty Int) `shouldBe` (fmap (+ 1) . fmap (* 2)) (1 :| [0, 3] :: NonEmpty Int)

  describe "applicative" $ do
    it "OK" $ pure id <*> (1 :| [2, 3] :: NonEmpty Int) `shouldBe` (1 :| [2, 3] :: NonEmpty Int)
    it "OK" $ pure (.) <*> ((+ 1) :| [] :: NonEmpty (Int -> Int)) <*> (+ 2) :| [] <*> 1 :| [] `shouldBe` ((+ 1) :| [] :: NonEmpty (Int -> Int)) <*> ((+ 2) :| [] <*> 1 :| [])
    it "OK" $ (pure (+ 1) :: NonEmpty (Int -> Int)) <*> pure 1 `shouldBe` pure 2
    it "OK" $ ((+ 2) :| [] :: NonEmpty (Int -> Int)) <*> pure 1 `shouldBe` pure ($ 1) <*> (+ 2) :| []

  describe "foldable" $ do
    it "OK" $ foldr (+) 0 (1 :| [2, 3] :: NonEmpty Int) `shouldBe` 6
    it "OK" $ foldr (*) 1 (1 :| [2, 3] :: NonEmpty Int) `shouldBe` 6
    it "OK" $ foldr (+) 3 (1 :| [4, 31, 40, (-50)] :: NonEmpty Int) `shouldBe` 29

  describe "traversable" $ do
    it "OK" $ traverse (: []) (1 :| [2, 3] :: NonEmpty Int) `shouldBe` [(1 :| [2, 3] :: NonEmpty Int)]

  describe "monad" $ do
    it "OK" $ ((1 :| [2, 3] :: NonEmpty Int) >>= return) `shouldBe` (1 :| [2, 3] :: NonEmpty Int)
    it "OK" $ ((1 :| [] :: NonEmpty Int) >>= (\x -> x :| [])) `shouldBe` (1 :| [] :: NonEmpty Int)
    it "OK" $ ((1 :| [2, 3] :: NonEmpty Int) >>= (\x -> x :| [- x])) `shouldBe` (1 :| [-1, 2, -2, 3, -3])
    it "OK" $ ((return 2 :: NonEmpty Int) >>= (\x -> x :| [-x])) `shouldBe` (\x -> x :| [-x]) 2
