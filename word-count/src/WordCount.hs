module WordCount (wordCount) where

import Data.Char (isAlpha, isDigit, toLower)

import qualified Data.Map as M

removeSingleQuote :: String -> String
removeSingleQuote word = (f . g) word
  where quote = '\''
        f s = if head s == quote then tail s else s
        g s = if last s == quote then reverse $ tail $ reverse s else s

wordCount :: String -> [(String, Int)]
wordCount xs = M.toList $ M.fromListWith (+) $ zip ws $ repeat 1
  where ws = map removeSingleQuote $ words $ normalize xs

normalize :: String -> String
normalize xs = map (f . toLower) xs
  where f c = if isAlpha c || isDigit c || c == '\'' then c else ' '
