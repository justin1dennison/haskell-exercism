module RunLength (decode, encode) where
import qualified Data.Char as C

import qualified Data.List as L

decode :: String -> String
decode [] = []
decode [x] = [x]
decode encodedText = concat (replicate times (take 1 ys)) ++ decode (drop 1 ys)
  where (xs, ys) = L.span C.isNumber encodedText
        times = if null xs then 1 else read xs :: Int

encode :: String -> String
encode text = concat $ zipWith zipper lengths letters
  where groupings = L.group text
        lengths = map (show . length) groupings
        letters = map (take 1) groupings
        zipper x y = if x == "1" then y else x ++ y

