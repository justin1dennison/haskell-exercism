module Prime (nth) where


nth :: Int -> Maybe Integer
nth n 
  | n <= 0 =  Nothing
  | otherwise = fmap f (Just [2..])
  where f = fromIntegral . last . take n . filter isPrime

isPrime :: Int -> Bool
isPrime n = not $ any ((== 0) . mod n) [2..k]
    where k = (floor . sqrt . fromIntegral) n
          
    

