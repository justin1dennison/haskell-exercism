module Series (Error(..), largestProduct) where

import Data.Char (digitToInt, isDigit)
import Data.List (find, tails, transpose)
import Data.Maybe (fromJust)

data Error = InvalidSpan
           | InvalidDigit Char
               deriving (Show, Eq)

isNotDigit :: Char -> Bool
isNotDigit = not . isDigit

windows :: Int -> [a] -> [[a]]
windows n = filter ((== n) . length) . transpose . take n . tails

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size == 0 = Right 1
  | size > length digits || size < 0 = Left InvalidSpan
  | any isNotDigit digits =
    Left (InvalidDigit $ fromJust $ find isNotDigit digits)
  | otherwise =
    Right $
      toInteger $ maximum $ map product $ windows size $ map digitToInt digits
