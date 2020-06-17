module IsbnVerifier (isbn) where

import Data.Char
import Data.Monoid

isValid :: Char -> Bool
isValid = getAny . foldMap (Any .) predicates
  where predicates = [isDigit, (== 'x')]

lowercase :: String -> String
lowercase = map toLower

parse :: Char -> Int
parse n
  | n == 'x' = 10
  | otherwise = digitToInt n

isbn :: String -> Bool
isbn
  = (== 0) .
      (`mod` 11) .
        sum .
          map uncurry (*) zip [10, 9 ..] .
            map parse . filter isValid . lowercase

