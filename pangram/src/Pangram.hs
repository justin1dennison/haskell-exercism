module Pangram (isPangram) where

import qualified Data.Char as C
import qualified Data.Set as S

isValid :: Char -> Bool
isValid c = C.isAlpha c && C.isAscii c

isPangram :: String -> Bool
isPangram text
  = (== 26) $ length $ S.fromList $ map C.toLower $ filter isValid text
