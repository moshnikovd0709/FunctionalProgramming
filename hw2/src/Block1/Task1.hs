-- | Contains a function for finding sum of numbers in the string
module Block1.Task1
  ( stringSum
  ) where

import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum s = sum <$> traverse readMaybe (words s)
