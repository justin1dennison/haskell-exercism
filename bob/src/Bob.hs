module Bob (responseFor) where

import qualified Data.Char as C

responseFor :: String -> String
responseFor xs 
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | not (any (not . C.isSpace) xs) = "Fine. Be that way!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

isYelling :: String -> Bool
isYelling xs  
  | null filtered = False
  | otherwise =  all C.isUpper $ filter C.isAlpha xs
  where filtered = filter C.isAlpha xs 

isQuestion :: String -> Bool
isQuestion xs = (== '?') $ last $ filter (not . C.isSpace) xs
