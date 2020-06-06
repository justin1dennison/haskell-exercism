module PerfectNumbers
  ( classify
  , Classification(..)
  ) where

data Classification
  = Deficient
  | Perfect
  | Abundant
  deriving (Eq, Show)

factors :: Int -> [Int]
factors n = filter (\x -> n `rem` x == 0) [1 .. n]

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | total < n = Just Deficient
  | total > n = Just Abundant
  | otherwise = Just Perfect
  where
    total = sum $ filter (/= n) $ factors n
