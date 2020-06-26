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

findInvalidDigit :: String -> Char
findInvalidDigit = fromJust . find isNotDigit

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size == 0 = Right 1
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | any isNotDigit digits = Left (InvalidDigit $ findInvalidDigit digits)
  | otherwise = Right (findMaxProduct digits)
  where products = map product . windows size . map digitToInt
        findMaxProduct = toInteger . maximum . products
