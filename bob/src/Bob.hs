module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)

isNotSpace :: Char -> Bool
isNotSpace = not . isSpace

responseFor :: String -> String
responseFor xs
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | not (any isNotSpace xs) = "Fine. Be that way!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

isYelling :: String -> Bool
isYelling xs
  | null filtered = False
  | otherwise = all isUpper filtered
  where filtered = filter isAlpha xs

isQuestion :: String -> Bool
isQuestion xs = (== '?') $ last $ filter isNotSpace xs
