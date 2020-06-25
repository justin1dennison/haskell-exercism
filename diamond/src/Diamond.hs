module Diamond (diamond) where

import Data.Char (isAlpha)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

charToString :: Char -> String
charToString = (: [])

letters :: String
letters = ['A' .. 'Z']

spaces :: [String]
spaces = map (`replicate` ' ') [1, 3 ..]

row :: Int -> String
row 0 = "A"
row n = c ++ dots ++ c
  where c = charToString $ letters !! n
        dots = spaces !! (n - 1)

endCap :: Int -> Int -> String
endCap total n = replicate (total - n + 1) ' '

diamond :: Char -> Maybe [String]
diamond c
  | not $ isAlpha c = Nothing
  | otherwise = Just $ zipWith (\ cap r -> cap ++ r ++ cap) caps rows

  where n = fromMaybe 0 $ elemIndex c letters
        seed = [0 .. n] ++ [n - 1, n - 2 .. 0]
        caps = map (endCap (n - 1)) seed
        rows = map row seed
