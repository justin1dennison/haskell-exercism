module Isogram
  ( isIsogram
  ) where

import Data.Char
import qualified Data.Set as S

isIsogram :: String -> Bool
isIsogram word = (length $ S.fromList $ map toLower w) == (length w)
  where
    w = filter isAlphaNum word
