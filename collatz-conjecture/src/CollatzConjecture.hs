module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = if n <= 0 then Nothing else Just $ collatz' n

collatz' :: Integer -> Integer
collatz' 1 = 0
collatz' n
  | even n = (+ 1) $ collatz' $ div n 2
  | otherwise = (+ 1) $ collatz' $ 3 * n + 1
