{-# LANGUAGE InstanceSigs #-}

-- | Contains binary search tree type and related methods.
module Block1.Task3
  (SearchTree (..)
  , isEmpty
  , size
  , contains
  , push
  , treeFromList
  , pop
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

-- | Binary search tree
data SearchTree x
  = Leaf
  | Node [x] (SearchTree x) (SearchTree x)
  deriving (Show)

instance Eq x => Eq (SearchTree x) where
  (==) :: SearchTree x -> SearchTree x -> Bool
  (==) Leaf Leaf = True
  (==) (Node x1 left1 right1) (Node x2 left2 right2) = (left1 == left2) && (right1 == right2) && (x1 == x2)
  (==) _ _ = False

-- | To calculate "List" size
lengthList                  :: [a] -> Int
lengthList []               =  0
lengthList (x:xs)           =  1 + length xs

isEmpty :: SearchTree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

size :: SearchTree a -> Int
size Leaf = 0
size (Node x left right) = size left + lengthList x + size right

contains :: Ord a => a -> SearchTree a -> Bool
contains _ Leaf = False
contains key (Node x left right)
  | key < (head x) = contains key left
  | key > (head x) = contains key right
  | otherwise = True

push :: Ord a => SearchTree a -> a -> SearchTree a
push Leaf x = (Node [x] Leaf Leaf)
push (Node n left right) x
  | head n < x = Node n left (push right x)
  | head n > x = Node n (push left x) right
  | otherwise = (Node (n ++ [x]) left right)

pop :: Ord a => a -> SearchTree a -> SearchTree a
pop _ Leaf = Leaf
pop x (Node n left right)
  | head n < x = Node n left (pop x right)
  | head n > x = Node n (pop x left) right
  | otherwise =
    if (lengthList n) > 1
          then Node (tail n) left right
          else merge left right
 where
    merge :: SearchTree a -> SearchTree a -> SearchTree a
    merge Leaf Leaf = Leaf
    merge left Leaf = left
    merge Leaf right = right
    merge (Node lE ll lr) (Node rE rl rr) =
      Node lE ll (Node rE (merge lr rl) rr)

-- | Realization of instance Foldable for SearchTree
instance Foldable SearchTree where
  foldMap :: Monoid m => (a -> m) -> SearchTree a -> m
  foldMap _ Leaf = mempty
  foldMap function (Node list left right) = foldMap function left `mappend` foldMap function list `mappend` foldMap function right

  foldr :: (a -> b -> b) -> b -> SearchTree a -> b
  foldr _ x Leaf = x
  foldr function x (Node list left right) = foldr function (foldr function (foldr function x right) list) left

-- | Constructor for Tree from 'List'
treeFromList :: Ord a => [a] -> SearchTree a
treeFromList = foldl push Leaf