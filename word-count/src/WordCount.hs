module WordCount (wordCount) where

import Data.Char (isAlphaNum)
import Data.Map.Strict (fromListWith, toList)
import Data.Monoid
import qualified Data.Text as T

shouldSplit :: Char -> Bool
shouldSplit = getAll . foldMap (All .) predicates
  where predicates = [not . isAlphaNum, (/= '\'')]

isEmpty :: T.Text -> Bool
isEmpty = (== 0) . T.length

combine :: [(T.Text, Int)] -> [(T.Text, Int)]
combine = toList . fromListWith (+)

wordCount :: T.Text -> [(T.Text, Int)]
wordCount
  = combine .
      frequencies .
        map (T.dropAround (== '\'')) .
          filter (not . isEmpty) . T.split shouldSplit

frequencies :: [T.Text] -> [(T.Text, Int)]
frequencies xs = [(T.toLower x, 1) | x <- xs]

