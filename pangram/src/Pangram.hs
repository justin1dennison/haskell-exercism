module Pangram (isPangram) where

import Data.Char (isAlpha, isAscii, toLower)
import Data.Set (fromList)

alphabet :: [Char]
alphabet = ['a' .. 'z']

isValid :: Char -> Bool
isValid c = isAlpha c && isAscii c

isPangram :: String -> Bool
isPangram text
  = (== length alphabet) $ length $ fromList $ map toLower $ filter isValid text
