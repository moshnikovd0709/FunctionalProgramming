{-# LANGUAGE InstanceSigs #-}

-- | Contains some 'Semigroup' realizations.
module Block3.Task2
  ( NonEmpty (..)
  , ThisOrThat (..)
  ) where

-- | Realization of Non empty list semigroup.
data NonEmpty a = a :| [a]
  deriving (Eq, Show)

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (x :| x') <> (y :| y') = x :| (x' ++ y : y')

-- | Realization of "at least on of two elements" semigroup.
data ThisOrThat a b = This a | That b | Both a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (This a) <> (Both _ b)    = Both a b
  (That b) <> (Both a _)    = Both a b
  (This a) <> (That b)      = Both a b
  (That b) <> (This a)      = Both a b
  (Both a b) <> (This _)    = Both a b
  (Both a b) <> (That _)    = Both a b
  (This a) <> (This _)      = This a
  (That a) <> (That _)      = That a
  (Both a b) <> (Both _ _)  = Both a b
