module Block2.Task1Spec
  ( spec,
  )
where

import Block2.Task1 (ArithmeticError (..), Expr (..), eval)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Expressions" $ do
    it "OK" $ eval (Add (Const 2) (Const 2)) `shouldBe` Right 4
    it "OK" $ eval (Sub (Const 4) (Const 1)) `shouldBe` Right 3
    it "OK" $ eval (Mul (Const 2) (Const 2)) `shouldBe` Right 4
    it "OK" $ eval (Div (Const 2) (Const 2)) `shouldBe` Right 1
    it "OK" $ eval (Pow (Const 2) (Const 4)) `shouldBe` Right 16
    it "OK" $ eval (Mul (Add (Const 5) (Const 5)) (Sub (Const 8) (Const 6))) `shouldBe` Right 20
    it "OK" $ eval (Div (Const 48) (Mul (Const 3) (Pow (Const 2) (Sub (Const 8) (Const 5))))) `shouldBe` Right 2
    it "OK" $ eval (Div (Const 5) (Const 0)) `shouldBe` Left DivisionByZero
    it "OK" $ eval (Div (Const 10) (Mul (Const 6) (Sub (Const 1) (Const 1)))) `shouldBe` Left DivisionByZero
    it "OK" $ eval (Pow (Const 1) (Const (-5))) `shouldBe` Left NegativePow
    it "OK" $ eval (Pow (Const 4) (Sub (Const 5) (Const (5)))) `shouldBe` Right 1
    it "OK" $ eval (Pow (Const 1) (Sub (Const 1) (Const 6))) `shouldBe` Left NegativePow
