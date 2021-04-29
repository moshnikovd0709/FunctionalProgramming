-- | Contains functions for splitting/joining lists with a separator.
module Block2.Task2
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

-- | Returns the result of splitting of current value with provided list
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr (split separator) ([] :| [])

split :: (Eq a) => a -> a -> NonEmpty [a] -> NonEmpty [a]
split separator x (head :| tail)
  | separator == x  = [] :| (head : tail)
  | otherwise     = ([x] ++ head) :| tail

-- | Returns the result of concatenation of current value with provided list
joinWith :: a -> NonEmpty [a] -> [a]
joinWith separator = foldl1 (join separator)

join :: a -> [a] -> [a] -> [a]
join x line = (++) (line ++ [x])



