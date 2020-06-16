module ProteinTranslation (proteins) where

isStop :: String -> Bool
isStop codon = codon `elem` ["UAA", "UAG", "UGA"]

encode :: String -> String
encode n
  | is ["AUG"] = "Methionine"
  | is ["UUU", "UUC"] = "Phenylalanine"
  | is ["UUA", "UUG"] = "Leucine"
  | is ["UCU", "UCC", "UCA", "UCG"] = "Serine"
  | is ["UAU", "UAC"] = "Tyrosine"
  | is ["UGU", "UGC"] = "Cysteine"
  | is ["UGG"] = "Tryptophan"
  | otherwise = "STOP"
  where is = elem n

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs
  | n > 0 = take n xs : chunksOf n (drop n xs)
  | otherwise = repeat []

proteins :: String -> Maybe [String]
proteins xs = Just $ map encode $ takeWhile (not . isStop) $ chunksOf 3 xs
