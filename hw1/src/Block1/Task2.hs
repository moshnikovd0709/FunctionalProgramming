{-# LANGUAGE InstanceSigs #-}

-- | Contains realization of different operations with numbers
module Block1.Task2
  ( Nat (..)
  , convertNatToInt
  , convertIntToNat
  ) where

-- | Natural numbers
data Nat
  = Z
  | S Nat
  deriving (Show)

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) Z x = x
  (+) y Z = y
  (+) (S x) y = S (x + y)

  (-) :: Nat -> Nat -> Nat
  (-) x Z = x
  (-) Z _ = Z
  (-) (S x) (S y) = x - y

  (*) :: Nat -> Nat -> Nat
  (*) _ Z = Z
  (*) Z _ = Z
  (*) x (S y) = x * y + x

-- | 4-6 tasks
instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    (==) Z Z = True
    (==) (S x) (S y) = x == y
    (==) Z _ = False
    (==) _ Z = False

instance Ord Nat where
    (>=) :: Nat -> Nat -> Bool
    (>=) _ Z = True
    (>=) (S x) (S y) = x >= y
    (>=) _ _ = False

convertIntToNat :: Int -> Nat
convertIntToNat x
  | x == 0    = Z
  | otherwise = S (convertIntToNat (x - 1))

convertNatToInt :: Nat -> Int
convertNatToInt Z = 0
convertNatToInt (S x) = convertNatToInt (x :: Nat) + 1
