module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  = if n <= 0 then Nothing else
      Just $ toInteger $ length $ takeWhile (/= 1) $ iterate collatz' n

collatz' :: Integer -> Integer
collatz' n
  | even n = div n 2
  | otherwise = 3 * n + 1
