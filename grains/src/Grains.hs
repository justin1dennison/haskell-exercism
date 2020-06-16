module Grains (square, total) where

import Data.Maybe (mapMaybe)

square :: Integer -> Maybe Integer
square n = if n <= 0 || n >= 65 then Nothing else Just (2 ^ (n - 1))

total :: Integer
total = sum $ mapMaybe square [1 .. 64]
