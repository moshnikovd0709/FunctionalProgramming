{-# LANGUAGE InstanceSigs #-}

-- | Contains arithmetic expression classes.
module Block2.Task1 (ArithmeticError (..), Expr (..), eval) where

data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

-- | Data for representing errors
data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving (Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x (Const 0)) = Left DivisionByZero
eval (Div (Const x) (Const y)) = Right (div x y)
eval (Div x y) = eval x >>= \x1 -> eval y >>= \y1 -> eval (Div (Const x1) (Const y1))
eval (Pow (Const x) (Const y)) =
 if y < 0
  then Left NegativePow
  else Right (x ^ y)
eval (Pow x y) = eval x >>= \x1 -> eval y >>= \y1 -> eval (Pow (Const x1) (Const y1))