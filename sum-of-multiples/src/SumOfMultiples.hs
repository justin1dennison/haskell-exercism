module SumOfMultiples (sumOfMultiples) where

import Data.Set (fromList)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = f $ map g factors
  where f = sum . fromList . concat
        g n = if n == 0 then [] else [0, n .. limit - 1]
