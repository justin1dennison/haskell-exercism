module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = f $ map g factors
  where f = sum . nub . concat
        g n = if n == 0 then [] else [0, n .. limit - 1]
