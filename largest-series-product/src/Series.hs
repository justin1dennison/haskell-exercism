module Series (Error(..), largestProduct) where

import Data.Char (digitToInt, isDigit)
import Data.List (find, tails, transpose)
import Data.Maybe (fromJust)

data Error = InvalidSpan
           | InvalidDigit Char
               deriving (Show, Eq)

windows :: Int -> [a] -> [[a]]
windows n = filter ((== n) . length) . transpose . take n . tails

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size == 0 = Right 1
  | size > length digits || size < 0 = Left InvalidSpan
  | (not . all isDigit) digits =
    Left (InvalidDigit $ fromJust $ find (not . isDigit) digits)
  | otherwise =
    Right $
      toInteger $ maximum $ map product $ windows size $ map digitToInt digits
