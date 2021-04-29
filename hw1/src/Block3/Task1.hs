-- | Contains function for joining 'Monoid's.
module Block3.Task1
  ( maybeConcat) where

-- | Returns the result of joining the lists in Maybe context
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldl f []

f :: [a] -> Maybe [a] -> [a]
f list Nothing = list
f list (Just x) = list ++ x