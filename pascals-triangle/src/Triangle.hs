module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows 1 = [[1]]
rows n = previous ++ [[1] ++ zipWith (+) lastRow (tail lastRow) ++ [1]]
  where previous = rows (n - 1)
        lastRow = last previous

