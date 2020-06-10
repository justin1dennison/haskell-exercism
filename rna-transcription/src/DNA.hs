module DNA (toRNA) where

encode :: Char -> Either Char Char
encode 'G' = Right 'C'
encode 'C' = Right 'G'
encode 'T' = Right 'A'
encode 'A' = Right 'U'
encode c = Left c

toRNA :: String -> Either Char String
toRNA = mapM encode
-- I started with
-- toRna = sequence . map 
-- however `hlint` recommended `mapM`
