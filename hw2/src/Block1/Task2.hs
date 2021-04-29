{-# LANGUAGE InstanceSigs #-}

-- | Contains instances for Tree data structure
module Block1.Task2
  ( Tree (..)
  ) where

-- | Tree representation
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

instance Applicative Tree where
  pure x = Leaf x
  (Leaf x) <*> f     = fmap x f
  (Branch left right) <*> f = Branch (left <*> f) (right <*> f)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f n (Leaf x)     = f x n
  foldr f n (Branch left right) = foldr f (foldr f n right) left

instance Traversable Tree where
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Branch left right) = Branch <$> traverse f left <*> traverse f right
