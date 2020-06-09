module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)

isNotSpace :: Char -> Bool
isNotSpace = not . isSpace

responseFor :: String -> String
responseFor xs
  | longSilence = "Fine. Be that way!"
  | yelled && questioned = "Calm down, I know what I'm doing!"
  | yelled = "Whoa, chill out!"
  | questioned = "Sure."
  | otherwise = "Whatever."
  where yelled = isYelling xs
        questioned = isQuestion xs
        longSilence = not (any isNotSpace xs)

isYelling :: String -> Bool
isYelling xs = (not . null) filtered && all isUpper filtered
  where filtered = filter isAlpha xs

isQuestion :: String -> Bool
isQuestion xs = (== '?') $ last $ filter isNotSpace xs
