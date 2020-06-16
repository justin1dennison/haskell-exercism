module ArmstrongNumbers (armstrong) where

import Data.Digits (digits)

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map f ds)
  where ds = digits 10 n
        len = length ds
        f d = d ^ len
