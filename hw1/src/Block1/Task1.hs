{-# LANGUAGE InstanceSigs #-}

-- | Contains different functions with days of week
module Block1.Task1
  ( NameOfDay (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data NameOfDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Eq NameOfDay where
  (==) :: NameOfDay -> NameOfDay -> Bool
  (==) Monday Monday       = True
  (==) Tuesday Tuesday     = True
  (==) Wednesday Wednesday = True
  (==) Thursday Thursday   = True
  (==) Friday Friday       = True
  (==) Saturday Saturday   = True
  (==) Sunday Sunday       = True
  (==) _ _                 = False


-- | To take mod from 7
mod7 :: Int -> Int
mod7 x | (7 > x)   = x
       | otherwise = mod7 (x - 7)

nextDay :: NameOfDay -> NameOfDay
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

-- | Convert Day to Integer value
dayToInt :: NameOfDay -> Int
dayToInt Monday    = 0
dayToInt Tuesday   = 1
dayToInt Wednesday = 2
dayToInt Thursday  = 3
dayToInt Friday    = 4
dayToInt Saturday  = 5
dayToInt Sunday    = 6

-- | Convert Integer value to Day
intToday :: Int -> NameOfDay
intToday 0 = Monday
intToday 1 = Tuesday
intToday 2 = Wednesday
intToday 3 = Thursday
intToday 4 = Friday
intToday 5 = Saturday
intToday 6 = Sunday

-- | Returns day of week that will be after given number of days
afterDays :: NameOfDay -> Int -> NameOfDay
afterDays d 0 = d
afterDays d x = intToday (mod7 ((dayToInt d) + x))

isWeekend :: NameOfDay -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Returns number of days from current to Friday
daysToParty :: NameOfDay -> Int
daysToParty Friday = 0
daysToParty d    = (daysToParty (intToday (mod7 ((dayToInt d) + 1)))) + 1
