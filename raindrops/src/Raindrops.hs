module Raindrops (convert) where

import Data.Maybe (mapMaybe)

isZero :: Int -> Bool
isZero = (== 0)

factors :: Int -> [Int]
factors n = filter (isZero . mod n) [1 .. n]

encode :: Int -> Maybe String
encode 3 = Just "Pling"
encode 5 = Just "Plang"
encode 7 = Just "Plong"
encode _ = Nothing

convert :: Int -> String
convert n = if null term then show n else term
  where fs = filter (\ c -> c `elem` [3, 5, 7]) $ factors n
        term = concat $ mapMaybe encode fs

